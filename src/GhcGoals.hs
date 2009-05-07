module Main where

import GHC
import GHC.Paths
import DynFlags (defaultDynFlags)
import MonadUtils

import Outputable
import PprTyThing

import System.Console.GetOpt (OptDescr (..), ArgDescr(..), getOpt, ArgOrder (..), usageInfo)
import System.Environment (getArgs)

import GoalCollector

data Config = Config { goalnames  :: [String]
                     } deriving (Show)

defaultConfig :: Config
defaultConfig = Config { goalnames = ["undefined"]
                       }

options :: [OptDescr (Config -> Config)]
options = [ Option ['g'] ["goal-names"] (ReqArg (\s c -> c { goalnames = [] }) "GOALS" ) "Specify the names of the goals. Default is \"undefined\"."
          ]

main :: IO ()
main = do
    let header = "Usage: ghc-goals FILENAME [OPTIONS...], with the following options:" 
    args <- getArgs
    (opts, files) <- processArgs defaultConfig options header args
    if null files 
      then putStrLn $ usageInfo header options
      else pprGoals =<< runGoals (head files) (goalnames opts)

processArgs :: a -> [OptDescr (a -> a)] -> String -> [String] -> IO (a, [String])
processArgs defaultConfig options header args =
    case getOpt Permute options args of
        (oargs, nonopts, []    ) -> return (foldl (flip ($)) defaultConfig oargs, nonopts)
        (_    , _      , errors) -> ioError $ userError $ (concat errors) ++ usageInfo header options

-- | Test the run goals without installing the patched ghci. 
--  Do rungoals "Test.hs" ["undefined","goalname2", ...]
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



