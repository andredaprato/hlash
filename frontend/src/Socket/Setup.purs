module Socket.Setup where

import Prelude

import Socket.Types
import WebSocket
import Data.Argonaut (Json, encodeJson, fromString, jsonParser, stringify)
import Data.Argonaut.Decode.Class (decodeJson)
import Data.Either (Either(..), either)
import Data.Maybe (Maybe(..))
import Effect.Aff.Class (class MonadAff)
import Effect.Var (($=))
import Halogen.Query.EventSource as ES
import Web.Socket.Event.MessageEvent (MessageEvent)


parseMessage :: forall m. MessageEvent -> Maybe MsgIn
parseMessage event = do
    let msg :: Either String Json
        msg  = jsonParser $ runMessage $ runMessageEvent event
    either 
      (\_ -> Nothing)

      (\json -> case decodeJson json :: Either String MsgIn  of
          Left _ -> do -- log "Failed to decode incoming ws message."
                       Nothing
          Right act -> Just act
           )  msg

onOpenEventSource (Connection socket) =  ES.effectEventSource \emitter -> do
  socket.onopen $= \event -> ES.emit emitter  unit
  pure mempty

messageEventSource :: forall m. MonadAff m => Connection -> ES.EventSource m MsgIn
messageEventSource (Connection socket) = ES.effectEventSource \emitter -> do
  socket.onmessage $= \event -> 
    case parseMessage event of
      Nothing -> pure unit
      Just msg -> ES.emit emitter  msg
  pure mempty
