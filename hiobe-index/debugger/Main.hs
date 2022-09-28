module Main where

import GHC.Debug.Client
import System.Environment
import GHC.Debug.Snapshot

snapshotName :: FilePath
snapshotName = "my-snapshot.snapshot"

main :: IO ()
main = getSnapshot
-- main = analyseSnapshot _ _

getSnapshot :: IO ()
getSnapshot = do
  sock <- getEnv "GHC_DEBUG_SOCKET"
  withDebuggeeConnect sock $ \d -> do
    makeSnapshot d "my-snapshot.snapshot"

analyseSnapshot :: DebugM a -> (a -> IO ()) -> IO ()
analyseSnapshot analysis k = do
  snapshotRun snapshotName $
    runAnalysis analysis k
