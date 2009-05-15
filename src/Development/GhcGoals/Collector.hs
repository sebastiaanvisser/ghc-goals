-- | This module contains the pure interface to goal collecting.
module Development.GhcGoals.Collector
  ( TypeSpec
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
import Data.Generics
import GHC.SYB.Instances
import GHC.SYB.Utils
import Type
import TypeRep
import GHC (typecheckedSource, TypecheckedModule)
import Control.Arrow((&&&))

-- | Type information on a term: its type, and predicated on type
-- variables.
type TypeSpec =
  ( [Type] -- Predicate information.
  , Type   -- The actual goal type.
  )

-- | Information on a goal: its name, position and type information.
type GoalInfo = (String, SrcSpan, TypeSpec)

-- | Analyze a type checked module, returning type information for all
-- variables with the specified names.
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
                           `extQ` predChangeCaseFunctionLevel dicts
                           `extQ` predChangeCaseEquationLevel dicts $ x
  where
    catQ r s x =  r x ++  s x
    topQuery = mkQ [] (collectGoalInfoVar goalNames loc dicts)

    recQuery :: GenericQ [GoalInfo]
    recQuery = concat . gmapQ (collectGoalInfo goalNames loc dicts)

    locChangeCase :: LHsExpr Id -> [GoalInfo]
    locChangeCase (L newloc child) = collectGoalInfo goalNames newloc dicts child
    excluded = False `mkQ` ((const True) :: NameSet -> Bool)

    predChangeCaseFunctionLevel :: [DictId] -> HsBind Id -> [GoalInfo]
    predChangeCaseFunctionLevel dicts (AbsBinds _ newdicts child1 child2) =
      let f :: GenericQ [GoalInfo]
          f = collectGoalInfo goalNames loc (dicts++newdicts)
      in f child1 ++ f child2
    predChangeCaseFunctionLevel _ x = recQuery x
    predChangeCaseEquationLevel :: [DictId] -> Match Id -> [GoalInfo]
    predChangeCaseEquationLevel dicts (Match pat child1 child2) = 
          let newdicts = dicts ++ concatMap collectPredPat pat
              f :: GenericQ [GoalInfo]
              f = collectGoalInfo goalNames loc newdicts
          in f child1 ++ f child2
    collectPredPat (L _ (ConPatOut _ _ newdicts _ _ _)) = newdicts
    collectPredPat x = []

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

{--
-- DEBUG STUFF

gshowsafe :: GenericQ String
gshowsafe =
  (\t ->
      "("
   ++ showConstr (toConstr t)
   ++ concat (gmapQ ((++) " " . gshowsafe) t)
   ++ ")"
  ) `extQ` (const "UNDEFINED" :: NameSet -> String)
    `extQ` (varNameString) `extQ` (lsts gshowsafe :: [LMatch Id] -> String)
 where
  lsts :: (a -> String) -> [a] -> String
  lsts s x = "["++lsts' s x++"]"
  lsts' :: (a -> String) -> [a] -> String
  lsts' s [] = ""
  lsts' s [x] = s x
  lsts' s (x:xs) = s x ++ " , " ++ lsts' s xs    

testquery :: GenericQ [String]
testquery = map gshow . map (varNameString&&&varType) . testquery'

testquery' :: GenericQ [Var]
testquery' = everythingBut excluded (++) [] ([] `mkQ` testqueryVar)
  where
    excluded :: GenericQ Bool
    excluded  = False `mkQ` ((const True) :: NameSet -> Bool)

testqueryVar ::  Var -> [Var]
testqueryVar = return
--}

