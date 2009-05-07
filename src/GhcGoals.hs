module Main where

import System.Console.GetOpt (OptDescr (..), ArgDescr(..), getOpt, ArgOrder (..), usageInfo)
import System.Environment (getArgs)

import Development.GhcGoals

data Config = Config { goalnames  :: [String]
                     } deriving (Show)

defaultConfig :: Config
defaultConfig = Config { goalnames = []
                       }

options :: [OptDescr (Config -> Config)]
options = [ Option ['g'] ["goal-names"] (ReqArg (\s c -> c { goalnames = goalnames c ++ words s }) "GOALS") "Specify the names of the goals. Default is \"undefined\"."
          ]

main :: IO ()
main = do
    let header = "Usage: ghc-goals FILENAME [OPTIONS...], with the following options:"
    args <- getArgs
    (opts, files) <- processArgs defaultConfig options header args
    let opts' = opts { goalnames = if null $ goalnames opts then ["undefined"] else goalnames opts }
    if null files
      then putStrLn $ usageInfo header options
      else pprGoals =<< getGoalsWith (goalnames opts') (head files)

processArgs :: a -> [OptDescr (a -> a)] -> String -> [String] -> IO (a, [String])
processArgs defaultConfig options header args =
    case getOpt Permute options args of
        (oargs, nonopts, []    ) -> return (foldl (flip ($)) defaultConfig oargs, nonopts)
        (_    , _      , errors) -> ioError $ userError $ (concat errors) ++ usageInfo header options
