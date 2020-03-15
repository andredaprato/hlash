module Page.Room where

import Prelude

import Capability.Navigate (class Navigate)
import Capability.Resource.Game (class ManageGame)
import Capability.Resource.Lobby (class ManageLobby, initGame, joinLobby, makeLobby)
import Capability.Resource.Question (class ManageQuestion, getQuestions)
import Capability.Resource.User (class ManageUser)
import Component.HOC.Connect as Conn
import Data.Array (length, replicate, take)
import Data.Const (Const(..))
import Data.Either (Either(..))
import Data.Game (Game)
import Data.List.Lazy (repeat, toUnfoldable)
import Data.List.Lazy as L
import Data.Lobby (Lobby, LobbyAction, LobbyError)
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype, unwrap)
import Data.Profile (Profile)
import Data.Question (Question)
import Data.RoomCode (RoomCode, validateRoomCode, writeRoomCode, toUpper)
import Data.Symbol (SProxy(..))
import Effect.Aff.Class (class MonadAff)
import Effect.Class.Console (log)
import Formless as F
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Page.DisplayMembers (renderMembers)
import Page.Game as Game
import Socket.Types (MsgIn(..))
import UI.UI (css)
import UI.Form as Form
import UI.Validation (FieldError, toText)

type State = { room :: Room
             , currentUser :: Profile
             , loading :: Boolean
             , userQuestions :: Maybe (Array Question)
             }


type ChildSlots = ( formless :: F.Slot CodeForm (Const Void) () Code Unit
                  , game :: H.Slot (Const Void) Void Unit
)

data Query a = Action MsgIn a 
data Action = NewLobby | JoinLobby Code | InitGame Lobby 
data Room = InLobby Lobby | InGame Game

component ::  forall m .
              MonadAff m  =>
              ManageQuestion m =>
              ManageLobby m =>
              ManageGame m =>
              Navigate m => 
              H.Component HH.HTML Query {room :: Room, currentUser :: Profile} Void m
component  = H.mkComponent { initialState : initialState
                           , render
                           , eval : H.mkEval $ H.defaultEval { handleAction = handleAction
                                                             , handleQuery = handleQuery}
                           }
  where
    initialState = \ {currentUser, room} -> { loading : false
                                            , room : room
                                            , userQuestions : Nothing
                                            , currentUser: currentUser
                                      }
    
    handleAction = case _ of
      NewLobby -> do
        makeLobby
        H.modify_ _ {loading = true}
      JoinLobby code -> do
        joinLobby $ toUpper  code.roomCode
        H.modify_ _ {loading = true}
      InitGame lobby -> initGame lobby 
                        
    handleQuery :: forall a. Query a -> H.HalogenM State Action _ _ m (Maybe a)
    handleQuery (Action res a )= case res of
      NewLobbyState lobby -> do
        userQuestions <- getQuestions

        H.liftEffect $ writeRoomCode lobby.lobbyCode

        H.modify_ _ {loading = false, room = InLobby lobby, userQuestions = userQuestions}
        pure (Just a)

      LobbyError err -> log "lobby error" *> H.modify_ _ {loading = false} *> pure (Just a)

      NewGameState game -> do
        H.modify_ _ { room = InGame game, loading = false}
        _ <- H.query (SProxy :: _ "game") unit $ (H.tell $ Game.Action (NewGameState game))
        pure (Just a) 


      _ -> log "uncaught pattern" *> H.modify_ _ {loading = false} *> pure (Just a) 


    render st =
      HH.div [css "container"]
      [
        case st.room of
          InGame game -> renderGame st game
          InLobby lobby -> renderLobby st lobby
      ]                            
                                      
    renderGame st game = HH.slot (SProxy :: _ "game") unit Game.component {game : game, currentUser : st.currentUser} (const Nothing)

    renderLobby st lobby =
        HH.div [css "section"]
        [
           HH.div [ css "tag is-large is-warning"]
           [
              
             HH.p [ css "has-text-centered is-size-7"] [ HH.text "Room Code: " ] 
           , HH.p [ css "is-size-6" ] [HH.text $ unwrap lobby.lobbyCode]
           ]

           -- 6  is max amount of people in the lobby
        , renderMembers $ take 6 $ map Just lobby.lobbyMembers <> (replicate 6 Nothing)

        , case st.currentUser == lobby.lobbyOwner of
            false -> HH.p [ css "is-size-6" ] [ HH.text "Waiting for lobby owner to start the game..." ] 
            true ->   HH.button [css "button is-link"
                               , HP.disabled $ length lobby.lobbyMembers <=  2
                               , HE.onClick \_ -> Just $ InitGame lobby
                               ]
                      [ HH.text "Start Game" ]
        ]

type Code = { roomCode :: RoomCode }
newtype CodeForm r f  = CodeForm ( r
                                   ( roomCode :: f FieldError String RoomCode
                                   )
                                 )
derive instance newtypeCodeForm :: Newtype (CodeForm r f) _
prx = F.mkSProxies (F.FormProxy :: _ CodeForm)

formComponent :: forall m . MonadAff m =>  F.Component CodeForm Query () Unit Code m
formComponent = F.component formInput $ F.defaultSpec { render = renderFormless
                                                      , handleEvent = F.raiseResult
                                                      }
  where
    formInput _ = { initialInputs : Nothing
                  , validators : CodeForm { roomCode : validateRoomCode
                                          }
                  }
    renderFormless st@{form} =
      HH.div [css "column"]
      [

       HH.form_
       [
         HH.div [ css "field" ]
         [
           HH.label [css "label"]
           [
             HH.text "Room code"
           ]
         ,
           HH.div [ css "control" ]
           [
             Form.input prx.roomCode form [ css "input is-uppercase is-size-6",
                                            HP.placeholder "Enter 4-letter room code"]
           , HH.p [css "help is-danger" ]
             [
              HH.text $  case F.getError prx.roomCode form of
                Nothing ->  ""
                Just a -> toText a
             ]
           ]
         ]
       , HH.div [ css "field" ]
        [
          HH.div [ css "control" ]
          [
            HH.button [ css "button is-link is-fullwidth is-medium",
                        HE.onClick \_ -> Just F.submit,
                        HP.type_ HP.ButtonSubmit]
            [ HH.text "Join Game"]
          ]
        ]
       ]
      ]
