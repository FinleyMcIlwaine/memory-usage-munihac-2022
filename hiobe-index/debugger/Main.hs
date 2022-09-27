module Main where

import GHC.Debug.Client
import GHC.Debug.Count
import GHC.Debug.Profile
import qualified Data.Map as Map

main :: IO ()
main = withDebuggeeConnect "ghc-debug.sock" debugger

debugger :: Debuggee -> IO ()
debugger = runAnalysis analysis1 print

printCensusByClosureType :: CensusByClosureType -> IO ()
printCensusByClosureType c = mapM_ putStrLn $ do
  (t,CS n s m) <- Map.toList c
  return $ "Type: " ++ show t ++ ": " ++ show n ++ ", " ++ show s ++ ", " ++ show m

-- The most basic analysis
analysis1 :: DebugM CensusStats
analysis1 = do
  rs <- gcRoots
  count rs

-- Census by closure type
analysis2 :: DebugM CensusByClosureType
analysis2 = do
  rs <- gcRoots
  censusClosureType rs

-- Census by closure type, 2 level!
analysis3 :: DebugM CensusByClosureType
analysis3 = do
  rs <- gcRoots
  census2LevelClosureType rs
