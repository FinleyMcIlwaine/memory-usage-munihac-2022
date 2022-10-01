module Main where

import GHC.Debug.Client
import GHC.Debug.Snapshot
import System.Environment
import Control.Concurrent (threadDelay)

snapshotName :: FilePath
snapshotName = "my-snapshot.snapshot"

main :: IO ()
-- main = getSnapshot
main = do
  sock <- getEnv "GHC_DEBUG_SOCKET"
  withDebuggeeConnect sock $ \d -> do
    putStrLn "pausing"
    pause d
    putStrLn "first pause done"
    putStrLn "resuming"
  threadDelay 1000000
  withDebuggeeConnect sock $ \d -> do
    putStrLn "pausing"
    pause d
    putStrLn "second pause done"
    putStrLn "resuming"
    putStrLn "done"


getSnapshot :: IO ()
getSnapshot = do
  sock <- getEnv "GHC_DEBUG_SOCKET"
  withDebuggeeConnect sock $ \d -> do
    makeSnapshot d "my-snapshot.snapshot"

analyseSnapshot :: DebugM a -> (a -> IO ()) -> IO ()
analyseSnapshot analysis k = do
  snapshotRun snapshotName $
    runAnalysis analysis k
