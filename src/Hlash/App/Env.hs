{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
module Hlash.App.Env where

import Data.Pool (Pool)
import Database.PostgreSQL.Simple as Sql (Connection)
import Hlash.Core.Profile
import Hlash.Core.Types
import Hlash.Effects.RoomCode
import Network.WebSockets as WS
import Servant.Auth.Server (JWTSettings, CookieSettings)
import System.Metrics (Store)
import System.Metrics.Distribution (Distribution)

type DbPool = Pool Sql.Connection
type Timings = IORef (HashMap Text Distribution)


data Client = Client {
  profile :: Profile,
  ws :: WS.Connection
  } deriving (Generic)


data RoomState = RoomState {
  clients :: TVar (HashMap Profile Client),
  lobbyStore :: LobbyStore,
  gameStore :: GameStore
  } deriving (Generic)

data Env (m :: Type -> Type) = Env
  { envDbPool         :: !DbPool
  , envTimings        :: !Timings
  , envEkgStore       :: !Store
  , envJWTSettings    :: !JWTSettings
  , envCookieSettings :: !CookieSettings
  , envRoomState      :: !RoomState
  }
  
class Has field env where
  obtain :: env -> field

instance Has DbPool                (Env m) where obtain = envDbPool 
instance Has Timings               (Env m) where obtain = envTimings
instance Has Store                 (Env m) where obtain = envEkgStore
instance Has JWTSettings           (Env m) where obtain = envJWTSettings
instance Has CookieSettings        (Env m) where obtain = envCookieSettings
instance Has RoomState             (Env m) where obtain = envRoomState




grab :: forall field env m . (MonadReader env m, Has field env) => m field
grab = asks $ obtain @field
{-# INLINE grab #-}  
