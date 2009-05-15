module Main where

import System.Console.GetOpt (OptDescr (..), ArgDescr(..), getOpt, ArgOrder (..), usageInfo)
import System.Environment (getArgs)
import Paths_ghc_goals (version)
import qualified Data.Version as V (showVersion)

import Development.GhcGoals

data Config = Config
                { goalnames  :: [String]
                , showVersion :: Bool
                } deriving (Show)

defaultConfig :: Config
defaultConfig = Config
                  { goalnames = []
                  , showVersion = False
                  }

options :: [OptDescr (Config -> Config)]
options =
  [ Option ['g'] ["goal-names"] (ReqArg (\s c -> c { goalnames = goalnames c ++ words s }) "GOALS") "Specify the names of the goals. Default is \"undefined\"."
  , Option ['V'] ["version"] (NoArg (\c -> c { showVersion = True })) "Show program version."
  ]

main :: IO ()
main = do
  let header = "Usage: ghc-goals FILENAME [OPTIONS...], with the following options:"
  args <- getArgs
  (opts, files) <- processArgs defaultConfig options header args
  if showVersion opts then printVersion else do
  let opts' = opts { goalnames = if null $ goalnames opts then ["undefined"] else goalnames opts }
  if null files
    then putStrLn $ usageInfo header options
    else mapM_ (goalsWith (goalnames opts')) files

printVersion :: IO ()
printVersion = putStrLn $ "ghc-goals version " ++ V.showVersion version

processArgs :: a -> [OptDescr (a -> a)] -> String -> [String] -> IO (a, [String])
processArgs defaultConfig options header args =
    case getOpt Permute options args of
        (oargs, nonopts, []    ) -> return (foldl (flip ($)) defaultConfig oargs, nonopts)
        (_    , _      , errors) -> ioError $ userError $ (concat errors) ++ usageInfo header options
