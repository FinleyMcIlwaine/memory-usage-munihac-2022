{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE ImportQualifiedPost #-}

{-# OPTIONS_GHC -Wno-type-defaults #-}

{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use newtype instead of data" #-}

module Main where

import Control.Monad
import Data.Map        qualified as LazyMap
import Data.Map.Strict qualified as StrictMap
import System.IO

import GHC.Debug.Stub

data StrictBox a = StrictBox !a

main :: IO ()
main = withGhcDebug $ do
  putStrLn "Enter a number:"
  num <- readLn

  -- example0 is just the number... is this a thunk?
  let example0 = num
  saveAndPause "example0" example0

  -- example1 is a thunk
  let example1 = length [0..num]
  saveAndPause "example1" example1

  -- example2 is forced to WHNF by the bang pattern
  let !example2 = length [0..num]
  saveAndPause "example2" example2

  -- do we expect example 0 to have changed now?
  saveAndPause "example0" example0

  -- A simple list structure
  let example3 = [0,1,2,3,4,5]
  saveAndPause "example3" example3

  -- A simple list structure as an arithmetic sequence, unforced
  let example4 = [0..5]
  saveAndPause "example4" example4

  -- A simple list structure, but as an arithmetic sequence, forced
  let !example5 = [0..5]
  saveAndPause "example5" example5

  -- A simple pair structure
  let example6 = ('a','b')
  saveAndPause "example6" example6

  -- Thunk... for now?
  let example8 = length [-100..num]

  -- A simple function with a strict field, called on a thunk
  let example7 !x = print x
  saveAndPause "example7" (example7 example8)

  -- Thunk?
  saveAndPause "example8" example8

  -- The internal structure of data structures can be inspected using
  -- ghc-debug, for example, we can see what the inside of a Map looks
  -- like.
  --
  -- A lazy map, forced to WHNF
  let !example9 = LazyMap.fromList $ zip [0..num] [1..num]
  saveAndPause "example9" example9

  -- A strict map, same definition
  let !example10 = StrictMap.fromList $ zip [0..num] [1..num]
  saveAndPause "example10" example10

  -- A strict map of Maybes?
  let !example11 = StrictMap.fromList $ zip [0..num] (map intToMaybe [1..num])
  saveAndPause "example11" example11

  -- StrictBox has a strict field, so forcing the value also causes the
  -- boxed value to be forced. This can be verified in ghc-debug
  let !example12 = StrictBox (length [0 .. num])
  saveAndPause "example12" example12

  -- Subtle point about strict fields! Doesn't mean we always avoid
  -- thunks. You have to make sure the constructor is forced in order
  -- to force the strict boxes. Just has a lazy field so
  let !example13 = Just (StrictBox (length [0 .. num]))
  saveAndPause "example13" example13

  putStrLn "Done"


intToMaybe :: Integer -> Maybe String
intToMaybe 42 = Nothing
intToMaybe n  = Just $ "was not 42... was " ++ show n

saveAndPause :: String -> a -> IO ()
saveAndPause desc x = do
  saveClosures [Box x]
  putStr $ "Pausing for " ++ desc ++ " (hit enter to continue)"
  hFlush stdout
  void getLine
