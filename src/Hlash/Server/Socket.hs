{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
module Hlash.Server.Socket where

import           Servant.Auth.Server.Internal.AddSetCookie

import           Hlash.App.Error (WithError, invalid, notAllowed, throwError, throwOnNothingM)
import           Hlash.Core.Password (PasswordHash(..), PasswordPlainText(..), mkPasswordHash, verifyPassword)
import           Hlash.Core.Profile (Profile(..))
import           Hlash.Core.Lobby
import           Hlash.Core.Types
import           Hlash.App.Env (grab, Has, Env, Client(..), RoomState(..))
import           Hlash.Db (WithDb, lookupUsername, insertUser, UserT(..), UserId)
import           Hlash.Db as Db

import           Hlash.Effects.Measure (MonadMeasure(..), timedAction)
import           Hlash.Time (dayInSeconds)
import           Hlash.Effects.RoomCode (RoomCode)
import           Hlash.Server.Types (AppServer, CookieHeaders, withAuthResult)
import           Data.HashMap.Strict as M

import qualified Data.Text as T
import           Servant.API.Generic (ToServant, AsApi)
import           Servant.Auth.Server
import           Servant (PostNoContent, Headers)
import           Servant.API.WebSocket
import           Network.WebSockets
import           Control.Concurrent
import           Control.Exception
import           Data.Aeson (encode, decode)
import           Hlash.Server.Socket.Message
import           Control.Monad.Trans.Control

data SocketSite route =
  SocketSite {
  socketRoute :: route :- "stream" :> WebSocket
            } deriving (Generic)

socketServer :: AuthResult UserId -> SocketSite AppServer
socketServer f = SocketSite { socketRoute = socketHandler  f } 

socketHandler :: ( MonadBaseControl IO m
                   , StM m () ~ ()
                   , WithDb env m
                   , MonadReader env m
                   , Has RoomState env
                   , MonadIO m
                   , WithError m
                   )
                => AuthResult UserId -> Connection ->  m ()
socketHandler (Authenticated (UserId user)) c  = streamData c
  where
  streamData c = flip finallyLifted disconnect  $ withPingThreadLifted c 30 (pure ())   $
    do 
      let client = Client { profile = Profile $ user, ws = c}   

      roomSt <- grab @RoomState
      liftIO $ atomically $ modifyTVar' (clients roomSt) (\clients -> M.insert (profile client) client clients )
      msgLoopWithTimeout client

   where disconnect = do st <- grab @RoomState
                         pure $ modifyTVar' (clients st) (\c -> M.delete (Profile $ show user) c)
socketHandler _ c = throwError $ notAllowed "unauthorized"




  
withPingThreadLifted :: (MonadBaseControl IO m,
                         StM m a ~ a) =>
                        Connection -> Int -> IO () -> m a -> m  a
withPingThreadLifted conn int ping act =
  control $ \runInIO -> withPingThread conn int ping (runInIO act)

finallyLifted ::  (MonadBaseControl IO m,
                         StM m a ~ a) =>
                  m a -> m b -> m a
finallyLifted act1 act2 = control $ \runInIO -> finally (runInIO act1) (runInIO act2)

-- | this Orphan instance is necessary for WebSockets to work as a protected endpoint
type instance AddSetCookieApi WebSocket = WebSocket
instance AddSetCookies ('S n) (m ()) (m ()) where
  addSetCookies _ a = a
