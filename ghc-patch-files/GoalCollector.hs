module GoalCollector (
    TypeSpec
  , GoalInfo
  , goalsFor
  ) where

import SrcLoc
import Var
import HsSyn
import NameSet
import Name hiding (varName)
import HsTypes
import Data.Data
import VarEnv
import SybUtils
import Type
import TypeRep
import GHC (typecheckedSource, TypecheckedModule)

type TypeSpec =
  ( [Type] -- ^ Predicate information.
  , Type   -- ^ The actual goal type.
  )

type GoalInfo = (String, SrcSpan, TypeSpec)

goalsFor :: TypecheckedModule -> [String] -> [GoalInfo]
goalsFor mod names =
     map (\(n, s, ts) -> (n, s, cleanupTypeSpec ts))
   $ collectGoalInfo names (error "no top-level SrcSpan found") [] (typecheckedSource mod)

cleanupTypeSpec :: TypeSpec -> TypeSpec
cleanupTypeSpec (preds, ty) = (map tidy preds, tidy ty)
  where tidy = tidyType emptyTidyEnv

collectGoalInfo :: [String] -> SrcSpan -> [DictId] -> GenericQ [GoalInfo]
collectGoalInfo goalNames loc dicts x
        | excluded x = []         
        | otherwise  = (topQuery `catQ` recQuery)
                                 `extQ` locChangeCase
                                 `extQ` predChangeCase $ x
     where catQ r s x =  r x ++  s x
           topQuery = mkQ [] (collectGoalInfoVar goalNames loc dicts)
           recQuery :: GenericQ [GoalInfo]
           recQuery = concat . gmapQ (collectGoalInfo goalNames loc dicts)
           locChangeCase :: LHsExpr Id -> [GoalInfo]
           locChangeCase (L newloc child) = collectGoalInfo goalNames newloc dicts child
           excluded  = False `mkQ` ((const True) :: NameSet -> Bool)
           predChangeCase :: HsBind Id -> [GoalInfo]
           predChangeCase (AbsBinds _ newdicts child1 child2) =
             let f :: GenericQ [GoalInfo]
                 f = collectGoalInfo goalNames loc newdicts
             in f child1 ++ f child2
           predChangeCase x = recQuery x

collectGoalInfoVar :: [String] -> SrcSpan -> [DictId] -> HsExpr Id -> [GoalInfo]
collectGoalInfoVar goalNames loc dicts ((HsWrap wrap (HsVar var)))
  | varNameString var `elem` goalNames = [(varNameString var, loc, typeSpec)]
  where typeSpec = (map varType dicts, reduceUnwrap wrap (varType var)) 
collectGoalInfoVar _ _ _ _             = []

varNameString :: Var -> String
varNameString var = occNameString . nameOccName . varName $ var

reduceUnwrap :: HsWrapper -> Type -> Type
reduceUnwrap wr ty = reduce_type $ unwrap wr ty

unwrap :: HsWrapper -> Type -> Type
unwrap WpHole t = t
unwrap (WpCompose w1 w2) t = unwrap w1 (unwrap w2 t)
unwrap (WpCast _) t = t -- XXX: really?
unwrap (WpTyApp t') t = AppTy t t'
unwrap (WpTyLam tv) t = ForAllTy tv t
-- do something else with coercion/dict vars?
unwrap (WpApp v) t = AppTy t (TyVarTy v)
unwrap (WpLam v) t = ForAllTy v t

-- | Reduce a top-level type application if possible. That is, we perform the
-- following simplification step:
-- @
-- (forall v . t) t' ==> t [t'/v]
-- @
-- where @[t'/v]@ is the substitution of @t'@ for @v@.
--
reduce_type :: Type -> Type
reduce_type (AppTy (ForAllTy tv b) t) = reduce_type (subst_type tv t b)
reduce_type (AppTy a               t) = reduce_type (AppTy (reduce_type a) (reduce_type t))
reduce_type t = t

subst_type :: TyVar -> Type -> Type -> Type
subst_type v t' t0 = go t0
  where
    go t = case t of
      TyVarTy tv
        | tv == v -> t'
        | otherwise -> t
      AppTy t1 t2 -> AppTy (go t1) (go t2)
      TyConApp c ts -> TyConApp c (map go ts)
      FunTy t1 t2 -> FunTy (go t1) (go t2)
      ForAllTy v' bt
        | v == v' -> t
        | otherwise -> ForAllTy v' (go bt)
      PredTy pt -> PredTy (go_pt pt)

   -- XXX: this is probably not right
    go_pt (ClassP c ts) = ClassP c (map go ts)
    go_pt (IParam i t) = IParam i (go t)
    go_pt (EqPred t1 t2) = EqPred (go t1) (go t2)
