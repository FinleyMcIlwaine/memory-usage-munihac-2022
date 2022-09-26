{-# LANGUAGE BangPatterns               #-}
{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE RecordWildCards            #-}

{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use <&>" #-}

module State where

import Control.Concurrent
import Control.Concurrent.STM
import Control.Monad.Reader
import Data.Map               (Map)
import Data.Map               qualified as Map
import Data.Text              (Text)
import Data.Text              qualified as T

import Database.SQLite.Simple

newtype HiobeM a = HiobeM { runHiobeM :: ReaderT (TVar HiobeState) IO a }
  deriving (Functor, Applicative, Monad, MonadIO, MonadReader (TVar HiobeState))

hiobeM :: MonadTrans t => HiobeM a -> t HiobeM a
hiobeM = lift

gets :: (HiobeState -> a) -> HiobeM a
gets g = asks readTVarIO >>= liftIO >>= return . g

modify :: (HiobeState -> HiobeState) -> HiobeM ()
modify g = ask >>= liftIO . atomically . flip modifyTVar g

modify' :: (HiobeState -> HiobeState) -> HiobeM ()
modify' !g = do
  !tvar <- ask
  liftIO . atomically $ modifyTVar' tvar g

data HiobeState =
  HiobeState
  { dbConn          :: MVar Connection
  , reqCount        :: Map Text Integer
  , langEngagements :: Map Text Integer
  }

initState :: MVar Connection -> HiobeState
initState db =
  HiobeState
  { dbConn = db
  , langEngagements = Map.empty
  , reqCount = Map.empty
  }

printState :: HiobeState -> IO ()
printState HiobeState{..} = do
  putStrLn "Request counts:"
  forM_ (Map.toList reqCount) $ \(p,c) ->
    putStrLn $ "Path " <> T.unpack p <> " was hit " <> show c <> " times"
  forM_ (Map.toList langEngagements) $ \(l,c) ->
    putStrLn $ "Language " <> T.unpack l <> " had " <> show c <> " engagements"

putLang :: Text -> HiobeM ()
putLang l =
  modify $ \HiobeState{..} ->
    HiobeState dbConn reqCount (Map.insertWith (+) l 1 langEngagements)

putReq :: Text -> HiobeM ()
putReq p = do
  modify $ \HiobeState{..} ->
    HiobeState dbConn (Map.insertWith (+) p 1 reqCount) langEngagements

