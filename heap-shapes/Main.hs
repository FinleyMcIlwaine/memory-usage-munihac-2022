{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE ImportQualifiedPost #-}

module Main where

import Control.Monad
import Data.Map        qualified as LazyMap
import Data.Map.Strict qualified as StrictMap
import System.IO

import GHC.Debug.Stub

main :: IO ()
main = withGhcDebug $ do
  putStr "Enter a number: "
  hFlush stdout
  num <- readLn

  -- example0 is just the number... is this a thunk?
  let example0 = num
  saveAndPause "example0" [Box example0]

  -- example1 is a thunk
  let example1 = length [0..num]
  saveAndPause "example1" [Box example1]

  -- example2 is forced to WHNF by the bang pattern,
  -- do we expect this to change example0?
  let !example2 = length [0..num]
  saveAndPause "example2" [Box example2, Box example0]

  -- A simple list structure as an arithmetic sequence, unforced
  let example3 = [num-2..num+2]
  saveAndPause "example3" [Box example3]

  -- A simple list structure, but as an arithmetic sequence, forced
  let !example4 = [num-2..num+2]
  saveAndPause "example4" [Box example4]

  -- Thunk... for now?
  let example6 = length [-100..num]

  -- A simple function with a strict field, called on a thunk
  let example5 !x = print x
  saveAndPause "example5" [Box $ example5 example6]

  -- Thunk?
  saveAndPause "example6" [Box example6]

  -- A lazy map, forced to WHNF
  let !example7 = LazyMap.fromList $ [(n-1,n+1) | n <- [0..num]]
  saveAndPause "example7" [Box example7]

  -- A strict map, same definition
  let !example8 = StrictMap.fromList $ [(n-1,n+1) | n <- [0..num]]
  saveAndPause "example8" [Box example8]

  -- A strict map of Maybes
  let !example9 = StrictMap.fromList $ [(n-1, integerToMaybe $ n+1) | n <- [0..num]]
  saveAndPause "example9" [Box example9]

  -- A strict map of strict Maybes
  let !example10 = StrictMap.fromList $ [(n-1, integerToStrictMaybe $ n+1) | n <- [0..num]]
  saveAndPause "example10" [Box example10]

  putStrLn "Done"

integerToMaybe :: Integer -> Maybe String
integerToMaybe 42 = Nothing
integerToMaybe n  = Just $ "was not 42... was " ++ show n

data Maybe' a = Nothing' | Just' !a

integerToStrictMaybe :: Integer -> Maybe' String
integerToStrictMaybe 42 = Nothing'
integerToStrictMaybe n  = Just' $ "was not 42... was " ++ show n


saveAndPause :: String -> [Box] -> IO ()
saveAndPause desc boxes = do
  saveClosures boxes
  putStr $ "Paused: " ++ desc ++ " (hit enter to continue)"
  hFlush stdout
  void getLine
