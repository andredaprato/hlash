module Socket.Game where


import Prelude
import Socket.Action

import Control.Monad.Reader (class MonadAsk, asks)
import Data.Game (GameAction(..), GameMsgIn(..))
import Data.Lobby (LobbyAction(..))
import Data.Maybe (Maybe, maybe)
import Data.Question (Question)
import Data.RoomCode (RoomCode(..))
import Effect.Aff.Class (class MonadAff)
import Effect.Class (liftEffect)
import Effect.Class.Console (log)
import Effect.Ref (Ref)
import Effect.Ref as Ref
import Env (UserEnv)
import Socket.Types (MsgOut(..))
import WebSocket (Connection(..), URL(..))


  
submitAnswer :: forall m r. MonadAff m => 
                MonadAsk { userEnv :: UserEnv ,
                           wsConn :: Ref (Maybe Connection) ,
                           wsUrl :: URL | r} m =>
 Question -> String -> RoomCode -> m Unit
submitAnswer question answer roomCode = do
  sendAction (GameMsg $ GameMsgIn (Response question answer) roomCode)

voteForQuestion question profile roomCode = do
  sendAction (GameMsg $ GameMsgIn (Vote question profile) roomCode)
  
