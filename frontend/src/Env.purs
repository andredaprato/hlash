module Env where

import Prelude

import WebSocket (Connection, URL)
import API.Request (BaseURL, WsURL)
import Data.Maybe (Maybe)
import Data.Profile (Profile)
import Data.RoomCode (RoomCode)
import Effect (Effect)
import Effect.Aff.Bus (BusRW)
import Effect.Ref (Ref)
import Effect.Var (GettableVar, SettableVar, Var)
import Halogen (HalogenIO)
import Web.Event.Event (Event)
import Web.Socket.Event.CloseEvent (CloseEvent)
import Web.Socket.Event.MessageEvent (MessageEvent)

type Env = 
  {
    baseUrl :: BaseURL
  , wsUrl :: URL
  , logLevel :: LogLevel 
  , userEnv :: UserEnv
  -- , wsConn :: Ref (Maybe Socket)
  , wsConn :: Ref (Maybe Connection)
  }
-- type Socket =   { binaryType     :: Var BinaryType
--                 , bufferedAmount :: GettableVar BufferedAmount
--                 , onclose        :: SettableVar (CloseEvent -> Effect Unit)
--                 , onerror        :: SettableVar (Event -> Effect Unit)
--                 , onmessage      :: SettableVar (MessageEvent -> Effect Unit)
--                 , onopen         :: SettableVar (Event -> Effect Unit)
--                 , protocol       :: Var Protocol
--                 , readyState     :: GettableVar ReadyState
--                 , url            :: GettableVar URL
--                 , close          :: Effect Unit
--                 , close'         :: Code -> Maybe Reason -> Effect Unit
--                 , send           :: Message -> Effect Unit
--                 , socket         :: GettableVar WebSocket
--   }

data LogLevel = Dev | Prod

derive instance eqLogLevel :: Eq LogLevel
derive instance ordLogLevel :: Ord LogLevel

type UserEnv =
  { currentUser :: Ref (Maybe Profile)
  , userBus :: BusRW (Maybe Profile)
  , roomCode :: Maybe RoomCode
  }
