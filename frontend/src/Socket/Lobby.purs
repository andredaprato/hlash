module Socket.Lobby where

import Prelude
import Socket.Action

import Control.Monad.Reader (class MonadAsk, asks)
import Data.Lobby (LobbyAction(..), Lobby)
import Data.Maybe (Maybe(..), maybe)
import Data.RoomCode (RoomCode)
import Effect.Aff.Class (class MonadAff)
import Effect.Class (liftEffect)
import Effect.Class.Console (log)
import Effect.Ref (Ref)
import Effect.Ref as Ref
import Env (UserEnv)
import Socket.Types (MsgOut(..))
import WebSocket (Connection(..), URL(..))

newLobby :: forall m r.
                MonadAff m => 
                MonadAsk { userEnv :: UserEnv ,
                           wsConn :: Ref (Maybe Connection) ,
                           wsUrl :: URL | r} m =>
   m Unit
newLobby = do
  currentUser <- liftEffect <<< Ref.read =<< asks _.userEnv.currentUser
  maybe (log "no user") (\profile -> sendAction (LobbyMsg $ NewLobby profile)) currentUser

joinLobby :: forall m r.
              MonadAff m => 
              MonadAsk { userEnv :: UserEnv ,
                         wsConn :: Ref (Maybe Connection) ,
                         wsUrl :: URL | r} m =>
   RoomCode -> m Unit
joinLobby roomCode = do
  currentUser <- liftEffect <<< Ref.read =<< asks _.userEnv.currentUser
  maybe (log "no user") (\profile -> sendAction (LobbyMsg $ JoinLobby roomCode profile)) currentUser

  
initGame :: forall m r.
              MonadAff m => 
              MonadAsk { wsConn :: Ref (Maybe Connection) ,
                         wsUrl :: URL | r} m =>
   Lobby -> m Unit
initGame lobby = sendAction (LobbyMsg $ InitGame lobby)

requestRoom :: forall m r.
                   MonadAff m => 
                   MonadAsk { userEnv :: UserEnv,
                              wsConn :: Ref (Maybe Connection) ,
                              wsUrl :: URL | r
                        } m =>
            m Unit
requestRoom = do
  roomCode <- asks _.userEnv.roomCode
  case roomCode of
    Nothing -> pure unit
    Just code -> sendAction (RequestRoom code)
