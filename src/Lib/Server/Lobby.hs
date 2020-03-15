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
module Lib.Server.Lobby where

import Servant.Auth.Server.Internal.AddSetCookie

import Lib.App.Error (WithError, invalid, notAllowed, throwError, throwOnNothingM)
import Lib.Core.Password (PasswordHash(..), PasswordPlainText(..), mkPasswordHash, verifyPassword)
import Lib.Core.Profile (Profile(..))
import Lib.Core.Lobby
import Lib.Core.Types
import Lib.App.Env (grab, Has, Env, Client(..), RoomState(..))
import Lib.Db (WithDb, lookupUsername, insertUser, UserT(..), UserId)
import Lib.Db as Db

import Lib.Effects.Measure (MonadMeasure(..), timedAction)
import Lib.Time (dayInSeconds)
import Lib.Effects.RoomCode (RoomCode)
import Lib.Server.Types (AppServer, CookieHeaders, withAuthResult)
import Data.HashMap.Strict as M

import qualified Data.Text as T
import Servant.API.Generic (ToServant, AsApi)
import Servant.Auth.Server 
import Servant (PostNoContent, Headers)
import Servant.API.WebSocket
import Network.WebSockets
import Control.Concurrent
import Control.Exception
import Data.Aeson (encode, decode)
import Lib.Server.Socket.Message
import Control.Monad.Trans.Control

data LobbySite route =
  LobbySite {
  newLobbyRoute :: route :- "stream" :> WebSocket
            } deriving (Generic)

lobbyServer :: AuthResult UserId -> LobbySite AppServer
lobbyServer f = LobbySite { newLobbyRoute = newLobbyHandler  f } 

newLobbyHandler :: ( MonadBaseControl IO m
                   , StM m () ~ ()
                   , WithDb env m
                   , MonadReader env m
                   , Has RoomState env
                   , MonadIO m
                   , WithError m
                   )
                => AuthResult UserId -> Connection ->  m ()
newLobbyHandler (Authenticated (UserId user)) c  = streamData c
  where
  streamData c = flip finallyLifted disconnect  $ withPingThreadLifted c 30 (pure ())   $
    do 
      let client = Client { profile = Profile $ user, ws = c}   

      roomSt <- grab @RoomState
      liftIO $ atomically $ modifyTVar' (clients roomSt) (\clients -> M.insert (profile client) client clients )
      msgLoopWithTimeout client

   where disconnect = do st <- grab @RoomState
                         pure $ modifyTVar' (clients st) (\c -> M.delete (Profile $ show user) c)
newLobbyHandler _ c = throwError $ notAllowed "unauthorized"




  
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
