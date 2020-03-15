module Socket.Action where

import Prelude

import Control.Monad.Reader (class MonadAsk, asks)
import Data.Argonaut (class EncodeJson, encodeJson, stringify)
import Data.Maybe (Maybe(..))
import Effect.Aff.Class (class MonadAff)
import Effect.Class (liftEffect)
import Effect.Class.Console (log)
import Effect.Ref (Ref)
import Effect.Ref as Ref
import WebSocket (Connection(..), Message(..), URL)


sendAction :: forall m r action.
              EncodeJson action =>
              MonadAff m => 
              MonadAsk {wsConn :: Ref (Maybe Connection) ,
                        wsUrl :: URL | r} m =>
           action -> m Unit
sendAction act = do
  mSocket <- liftEffect <<< Ref.read  =<< asks _.wsConn
  case mSocket of
    Nothing -> log "no socket"
    Just (Connection socket) ->
       liftEffect $ void $  socket.send (Message <<< stringify <<< encodeJson $ act)
