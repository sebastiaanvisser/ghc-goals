-- | This module contains a high level interface to the goals
-- functionality. Given a file, it takes care of parsing, typechecking
-- etc., producing type information on all the goals.
module Development.GhcGoals (
      goals
    , goalsWith
    , pprGoals
    , pprTypeSpecForUser
    ) where

import GHC (defaultErrorHandler, load, LoadHowMuch(..), runGhc, getSessionDynFlags, setSessionDynFlags, guessTarget, setTargets, depanal, parseModule, typecheckModule, dopt, DynFlag(Opt_PrintExplicitForalls))
import GHC.Paths (libdir)
import DynFlags (defaultDynFlags)
import MonadUtils (liftIO)

import Outputable (showSDocForUser, neverQualify, hsep, text, nest, dcolon, (<+>), ppr, empty, parens, pprWithCommas)
import PprTyThing (pprTypeForUser)

import Development.GhcGoals.Collector

-- | Analyze a file, returning type information for all 'undefined's.
goals :: FilePath -> IO [GoalInfo]
goals = goalsWith ["undefined"]

-- | Analyze a file, returning type information for all variables with
-- the specified names.
goalsWith :: [String] -> FilePath -> IO [GoalInfo]
goalsWith goals file =
    defaultErrorHandler defaultDynFlags $
      runGhc (Just libdir) $ do
        dflags   <- getSessionDynFlags
        setSessionDynFlags dflags
        target   <- guessTarget file Nothing
        setTargets [target]
        load LoadAllTargets
        (md:mds) <- depanal [] True
        pm       <- parseModule md
        tcm      <- typecheckModule pm
        return $ goalsFor tcm goals

-- | Pretty print information on goals in a style similar to GHCi.
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



