module OfflineTestGoalCollector where
import GHC
import GoalCollector


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
        (sum:sums) <- depanal [] True
        pm         <- parseModule sum
        tcm        <- typecheckModule pm
        let wraps = collectWraps goals (error nosrcspan ::SrcSpan) [] . typecheckedSource $ tcm
        liftIO . print $ map (showSDocForUser neverQualify . ppr) wraps

nosrcspan = "There is no location info! Everything is crashing!"




