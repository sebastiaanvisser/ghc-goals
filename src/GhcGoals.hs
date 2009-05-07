module GhcGoals where

import GHC
import GHC.Paths
import DynFlags
import MonadUtils

import Outputable
import PprTyThing

import GoalCollector

import GHC.SYB.Instances
import Data.Generics

-- | Test the run goals without installing the patched ghci. 
--  Do rungoals "Test.hs" ["undefined","goalname2", ...]
rungoals :: FilePath -> [String] -> IO ()
rungoals file goals = 
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
        let wraps = goalsFor tcm goals
            pefas = dopt Opt_PrintExplicitForalls dflags
            showWrap (n, s, ts) = showSDocForUser neverQualify $ hsep [text n, nest 2 (dcolon <+> pprTypeSpecForUser pefas ts), text " -- Used in", ppr s]
        liftIO $ mapM_ (putStrLn.showWrap) wraps


pprTypeSpecForUser pefas (ts, ty) =
  (if null ts
    then empty
    else parens (pprWithCommas (pprTypeForUser pefas) ts) <+> text "=>")
  <+> pprTypeForUser pefas ty



