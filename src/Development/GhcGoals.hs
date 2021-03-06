-- | This module contains a high level interface to the goals
-- functionality. Given a file, it takes care of parsing, typechecking
-- etc., producing type information on all the goals.
module Development.GhcGoals
  ( goals
  , goalsWith
  , getGoals
  , getGoalsWith
  , pprGoals
  ) where

import Control.Monad (liftM)
import Data.List (sortBy)
import Data.Ord (comparing)

import GHC
  ( defaultErrorHandler
  , depanal
  , dopt
  , DynFlag(Opt_PrintExplicitForalls)
  , getSessionDynFlags
  , ghcLink
  , GhcLink(..)
  , guessTarget
  , handleSourceError
  , hscTarget
  , HscTarget(..)
  , load
  , LoadHowMuch(..)
  , parseModule
  , printExceptionAndWarnings
  , runGhc
  , setSessionDynFlags
  , setTargets
  , SuccessFlag(..)
  , typecheckModule
  )
import GHC.Paths (libdir)
import DynFlags (defaultDynFlags)
import MonadUtils (liftIO)

import Outputable
  ( (<+>)
  , dcolon
  , empty
  , hsep
  , nest
  , neverQualify
  , parens
  , ppr
  , pprWithCommas
  , showSDocForUser
  , text
  )
import PprTyThing (pprTypeForUser)

import Development.GhcGoals.Collector

-- | Analyze a file, print type information for all 'undefined's
goals :: FilePath -> IO ()
goals = goalsWith ["undefined"]

-- | Analyze a file, print type information for all variables with the
-- specified names.
goalsWith :: [String] -> FilePath -> IO ()
goalsWith goals file = getGoalsWith goals file >>= pprGoals

-- | Analyze a file, returning type information for all 'undefined's.
getGoals :: FilePath -> IO [GoalInfo]
getGoals = getGoalsWith ["undefined"]

-- | Analyze a file, returning type information for all variables with
-- the specified names.
getGoalsWith :: [String] -> FilePath -> IO [GoalInfo]
getGoalsWith goals file =
  defaultErrorHandler defaultDynFlags $
    runGhc (Just libdir) $ handleSourceError errorHandler $ do
      dflags   <- getSessionDynFlags
      setSessionDynFlags $ dflags
        { ghcLink = LinkInMemory
        , hscTarget = HscNothing -- Interpreted
        }
      target   <- guessTarget file Nothing
      setTargets [target]
      success <- load LoadAllTargets
      case success of
        Succeeded -> do
          (md:mds) <- depanal [] True
          pm       <- parseModule md
          tcm      <- typecheckModule pm
          return . sortBy (comparing snd3) $ goalsFor tcm goals
        Failed     -> return []
  where
    errorHandler x = printExceptionAndWarnings x >> return []

snd3 :: (a, b, c) -> b
snd3 (a, b, c) = b

-- | Pretty print information on goals in a style similar to GHCi.
pprGoals :: [GoalInfo] -> IO ()
pprGoals goals = do
    defaultErrorHandler defaultDynFlags $
      runGhc (Just libdir) $ do
        dflags     <- getSessionDynFlags
        let pefas = dopt Opt_PrintExplicitForalls dflags
            showWrap (n, s, ts) = showSDocForUser neverQualify
                                    $ hsep [ text n
                                           , nest 2 (dcolon <+> pprTypeSpecForUser pefas ts)
                                           , text " -- Used in"
                                           , ppr s]
        liftIO $ mapM_ (putStrLn . showWrap) goals

pprTypeSpecForUser pefas (ts, ty) =
  (if null ts
   then empty
   else parens (pprWithCommas (pprTypeForUser pefas) ts) <+> text "=>")
  <+> pprTypeForUser pefas ty

