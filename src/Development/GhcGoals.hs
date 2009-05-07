module Development.GhcGoals (
      runGoals
    , pprGoals
    , pprTypeSpecForUser
    ) where

import GHC (defaultErrorHandler, runGhc, getSessionDynFlags, setSessionDynFlags, guessTarget, setTargets, depanal, parseModule, typecheckModule, dopt, DynFlag(Opt_PrintExplicitForalls))
import GHC.Paths (libdir)
import DynFlags (defaultDynFlags)
import MonadUtils (liftIO)

import Outputable (showSDocForUser, neverQualify, hsep, text, nest, dcolon, (<+>), ppr, empty, parens, pprWithCommas)
import PprTyThing (pprTypeForUser)

import Development.GhcGoals.Collector

runGoals :: FilePath -> [String] -> IO [GoalInfo]
runGoals file goals = 
    defaultErrorHandler defaultDynFlags $ 
      runGhc (Just libdir) $ do
        dflags     <- getSessionDynFlags
        setSessionDynFlags dflags
        target     <- guessTarget file Nothing
        setTargets [target]
        --load LoadAllTargets
        (md:mds) <- depanal [] True
        pm         <- parseModule md
        tcm        <- typecheckModule pm
        return $ goalsFor tcm goals

pprGoals :: [GoalInfo] -> IO ()
pprGoals goals = do
    defaultErrorHandler defaultDynFlags $ 
      runGhc (Just libdir) $ do
        dflags     <- getSessionDynFlags
        let pefas = dopt Opt_PrintExplicitForalls dflags
            showWrap (n, s, ts) = showSDocForUser neverQualify $ hsep [text n, nest 2 (dcolon <+> pprTypeSpecForUser pefas ts), text " -- Used in", ppr s]
        liftIO $ mapM_ (putStrLn . showWrap) goals

pprTypeSpecForUser pefas (ts, ty) =
  (if null ts
    then empty
    else parens (pprWithCommas (pprTypeForUser pefas) ts) <+> text "=>")
  <+> pprTypeForUser pefas ty



