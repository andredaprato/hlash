module Component.Router where

import Prelude

import Capability.Navigate (class Navigate, navigate)
import Capability.Resource.Game (class ManageGame)
import Capability.Resource.Lobby (class ManageLobby)
import Capability.Resource.Question (class ManageQuestion)
import Capability.Resource.User (class ManageUser)
import Component.HOC.Connect (WithCurrentUser)
import Component.HOC.Connect as Connect
import Control.Alternative ((<|>))
import Control.Monad.Reader (class MonadAsk, asks)
import Data.Argonaut (Json, decodeJson, jsonParser)
import Data.Array (elem)
import Data.Const (Const(..))
import Data.Either (Either(..), either, hush)
import Data.Lobby (LobbyAction)
import Data.Maybe (Maybe(..), fromMaybe, isJust, isNothing)
import Data.Profile (Profile)
import Data.Route (Route(..), router)
import Data.String (toLower)
import Data.Symbol (SProxy(..))
import Effect (Effect)
import Effect.Aff (Aff, Milliseconds(..), delay, launchAff)
import Effect.Aff.Class (class MonadAff)
import Effect.Class (liftEffect)
import Effect.Class.Console (log)
import Effect.Ref (Ref)
import Effect.Ref as Ref
import Effect.Var (($=))
import Env (UserEnv, Env)
import Halogen as H
import Halogen.Aff as HA
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Halogen.HTML.Properties.ARIA as ARIA
import Page.Login as Login
import Page.Home as Home
import Page.Room as Room
import Page.Question as Question
import Page.Register as Register
import Routing.Duplex (RouteDuplex', parse, path, root, segment, string)
import Routing.Duplex as RD
import Routing.Duplex.Generic as G
import Routing.Duplex.Generic.Syntax ((/))
import Routing.Hash (getHash, matches, matchesWith)
import Routing.Match (Match, lit, num, str, end)
import Socket.Lobby (requestRoom)
import Socket.Setup (messageEventSource, onOpenEventSource)
import Socket.Types (MsgIn(..))
import UI.UI (css)
import WebSocket (Connection(..), URL(..), WebSocket, newWebSocket, runMessage, runMessageEvent)

type ChildSlots =
  (
    login :: H.Slot (Const Void) Unit Unit
  , button :: H.Slot (Const Void) Void Unit
  , room :: H.Slot Room.Query Void Unit
  , article :: H.Slot (Const Void) Void Unit
  , register :: H.Slot (Const Void) Void Unit
  , question :: H.Slot (Const Void) Void Unit
  , home :: H.Slot Home.Query Void Unit
  )

type State = { currentPage :: Maybe Route
             , currentUser :: Maybe Profile
             , loading :: Boolean
             , room :: Maybe Room.Room
             }


             
data Query a = GoTo Route a
data Action = Receive { | WithCurrentUser ()} 
            | Initialize
            | ConnectSocket
            | FinishLoading Unit
            | ReceiveMessage MsgIn




component ::  forall m r.
              MonadAff m =>
              ManageUser m =>
              ManageQuestion m =>
              Navigate m =>
              ManageLobby m =>
              ManageGame m =>
              MonadAsk Env m =>
              H.Component HH.HTML Query  {} Void m
component = Connect.component $ H.mkComponent { initialState : initialState
                                              , render
                                              , eval : H.mkEval $ H.defaultEval { handleQuery = handleQuery
                                                                                , handleAction = handleAction
                                                                                , initialize = Just Initialize
                                                                                , receive = Just <<< Receive
                                                                                }
                                              }
  where
    initialState = \ {currentUser} -> {currentPage : Just Home, currentUser
                                      , loading : false
                                      , room : Nothing}

    handleAction :: Action -> H.HalogenM State Action ChildSlots Void m Unit
    handleAction = case _ of
      Initialize ->  do
        st <- H.get
        case st.currentUser of
          Nothing ->  pure unit
          Just s -> handleAction ConnectSocket

        -- first we'll get the route the user landed on
        initialRoute <- hush <<< (RD.parse router) <$> liftEffect getHash
        -- then we'll navigate to the new route (also setting the hash)
        navigate $ fromMaybe Home initialRoute

      Receive { currentUser }  -> do st <- H.get
                                     H.modify_ _ { currentUser = currentUser } 
                                     case (isNothing st.currentUser && isJust currentUser ) of
                                       false -> pure unit
                                       _ -> handleAction ConnectSocket
      ConnectSocket -> do
        wsUrl <-  asks _.wsUrl
        ref <- asks _.wsConn
        socket <- liftEffect $ newWebSocket (wsUrl) []
        H.modify_ _ {loading = true}
        _ <- H.subscribe (FinishLoading <$> onOpenEventSource socket)
        _ <- H.subscribe (ReceiveMessage <$> messageEventSource socket)
        liftEffect $ Ref.write (Just socket) ref
          
      FinishLoading _ -> H.modify_ _ {loading = false} *> requestRoom

      ReceiveMessage act -> do
        st <- H.get
        case st.room of
          Nothing ->  case act of 
            LobbyError err -> do _ <- H.query (SProxy :: _ "home" ) unit $ (H.tell $ Home.Error err)  
                                 pure unit
      

            NewLobbyState lobby -> do H.modify_ _ { room = Just (Room.InLobby lobby) }
                                      _ <- H.query (SProxy :: _ "room") unit $ (H.tell $ Room.Action act)
                                      pure unit
                                      
            NewGameState game -> do H.modify_ _ { room = Just (Room.InGame game) }
                                    _ <- H.query (SProxy :: _ "room") unit $ (H.tell $ Room.Action act)
                                    pure unit
            _ -> log "game errors not yet handled :("

          _ -> do _ <- H.query (SProxy :: _ "room") unit $ (H.tell $ Room.Action act)
                  pure unit


 

    handleQuery :: forall a . Query a -> H.HalogenM State Action ChildSlots Void m (Maybe a)
    handleQuery = case _ of
      GoTo path  next -> do
        { currentPage, currentUser }  <- H.get 
        when  (currentPage /= Just path) $ do
           case (isJust currentUser && path `elem` [ Login, Register ]) of
             false -> H.modify_ _ { currentPage = Just path }
             _ ->  H.modify_ _ { currentPage = Just Home }
        pure $ Just next
  

    authorize :: Maybe Profile -> (Profile -> H.ComponentHTML Action ChildSlots m) -> H.ComponentHTML Action ChildSlots m
    authorize mbProfile html = case mbProfile of
      Nothing ->
        HH.slot (SProxy :: _ "login") unit Login.component unit (const $  Just ConnectSocket)
      Just p ->
        html p

    render st =  case st.loading of
      false -> renderPage st
      true -> HH.h1 [ css "title" ] [ HH.text "Loading... "]

    renderPage st = 
      HH.div_ 
        [
       case st.room of
           Nothing -> renderCurrentPage st
           Just room -> authorize st.currentUser $ \p -> HH.slot (SProxy :: _ "room") unit Room.component { currentUser : p
                                                                                                          , room : room } (\_ -> Nothing)
        ]
    renderCurrentPage st = 
       HH.div_ [
       case st.currentPage of
           Just Login    ->
             HH.slot (SProxy :: _ "login")    unit Login.component unit (const $ Just ConnectSocket)
           Just Register -> 
             HH.slot (SProxy :: _ "register") unit Register.component unit (\_ -> Nothing)
           Just Home     -> 
             authorize st.currentUser $ \p -> HH.slot (SProxy :: _ "home")    unit Home.component {currentUser : p} (\_ -> Nothing)
           _             ->
             HH.div_ [ HH.text "Sorry we can't find that page!"]
        ]
