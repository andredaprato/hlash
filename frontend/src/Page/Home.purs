module Page.Home where


import Prelude

import Capability.Navigate (class Navigate, logout)
import Capability.Resource.Lobby (class ManageLobby, joinLobby, makeLobby)
import Capability.Resource.Question (class ManageQuestion)
import Capability.Resource.User (class ManageUser, logoutUser)
import Data.Const (Const(..))
import Data.Game (Game)
import Data.Lobby (LobbyError(..))
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype)
import Data.Profile (Profile)
import Data.Question (Question, toString)
import Data.RoomCode (RoomCode, removeRoomCode, validateRoomCode, toUpper)
import Data.Route (Route(..))
import Data.Symbol (SProxy(..))
import Data.Tuple (fst, snd)
import Data.Username (toString)
import Data.Username as User
import Effect.Aff (Milliseconds(..), delay)
import Effect.Aff.Class (class MonadAff)
import Effect.Class.Console (log, logShow)
import Formless as F
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Lib.Page.DisplayAnswers as DisplayAnswers
import Page.DisplayMembers (renderMembers)
import Page.GameOver (renderGameOver)
import Page.Question as Question
import Socket.Types (MsgIn(..))
import UI.Form (control, field)
import UI.Form as Form
import UI.UI (column, css)
import UI.Validation (FieldError(..), maxLength, toText)

type State =
  {
    currentUser :: Profile
  }

data Action = JoinLobby Code | NewLobby | Logout
data Query a = Error LobbyError a

type ChildSlots = ( formless :: F.Slot CodeForm FormQuery () Code Unit
                  , question :: H.Slot (Const Void) Void Unit
                  )
component ::  forall m .
              MonadAff m  =>
              ManageLobby m =>
              ManageUser m =>
              ManageQuestion m =>
              Navigate m =>
              H.Component HH.HTML Query {currentUser :: Profile} Void m
component  = H.mkComponent { initialState : initialState
                             , render
                             , eval : H.mkEval $ H.defaultEval { handleAction = handleAction
                                                               }
                             }
  where
    initialState = \ { currentUser } -> { currentUser }

    handleAction  = case _ of
      JoinLobby code -> do
        joinLobby $ toUpper  code.roomCode
      NewLobby -> do
        makeLobby
      Logout -> do logoutUser *> logout -- logout on backend, and then modify userEnv on frontend
                   

    handleQuery :: forall a . Query a -> H.HalogenM State Action ChildSlots Void m (Maybe a)
    handleQuery = case _ of
      Error err next -> do _ <- H.query F._formless unit $ F.injQuery  $ CodeError err unit
                           pure $ Just next

    render st =
      HH.div []
      [
        HH.section [ css "hero is-dark is-bold" ]
        [
          HH.div [css "hero-body" ]
          [
            HH.div [ css "container" ]
            [
              HH.div [ css "level" ]
            [
              HH.div [ css "level-left"]
              [
                HH.h1 [ css "title"] [HH.text "Hlash"]
              ]
            , HH.div [ css "level-right"]
              [
                HH.p [css "navbar-item is-size-4"] [ HH.text $ User.toString st.currentUser.username]
              , HH.a [ css "navbar-item is-size-5", HE.onClick \_ -> Just Logout ]
                [
                  HH.text "Logout"
                ]
              ]
            ]
            ]
          ]
        ]
      , HH.div [css "section"]
        [
          HH.div [ css "columns" ]
          [
            HH.article [ css "column is-7" ]
            [
              HH.div [ css "box" ]
              [
                HH.h1 [ css "title" ] [ HH.text "Create or join a new game"]
              , HH.slot F._formless unit formComponent unit (Just <<< JoinLobby)
              , HH.a [ HE.onClick \_ -> (Just NewLobby) ]
                [ HH.text "Start a new game." ]
              ]
              
            ]
          , HH.div [ css "column" ]
            [
              HH.div [ css "box"]
              [
                HH.slot (SProxy :: _ "question") unit Question.component unit (\_ -> Nothing)
              ]
            ]
          ]
        ]
      ]


type Code = { roomCode :: RoomCode }
newtype CodeForm r f  = CodeForm ( r
                                   ( roomCode :: f FieldError String RoomCode
                                   )
                                 )
derive instance newtypeCodeForm :: Newtype (CodeForm r f) _
prx = F.mkSProxies (F.FormProxy :: _ CodeForm)

data FormQuery a = CodeError  LobbyError a
derive instance functorQuery :: Functor FormQuery 

formComponent :: forall m . MonadAff m =>  F.Component CodeForm FormQuery () Unit Code m
formComponent = F.component formInput $ F.defaultSpec { render = renderFormless
                                                      , handleEvent = F.raiseResult
                                                      , handleQuery = handleQuery}
  where
    formInput _ = { initialInputs : Nothing
                  , validators : CodeForm { roomCode : validateRoomCode
                                          }
                  , codeError : Nothing
                  }

    handleQuery :: forall a. FormQuery a -> H.HalogenM _ _ _ _ _  (Maybe a)
    handleQuery = case _ of 
      CodeError err a -> do
        log "here"
        H.modify_ _ { codeError = Just err}
        pure (Just a)

    renderFormless st@{form} =

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
       , HH.div [ css "field is-grouped" ]
        [
          HH.div [ css "control" ]
          [
            HH.button [ css "button is-link is-fullwidth is-medium",
                        HE.onClick \_ -> Just F.submit,
                        HP.type_ HP.ButtonSubmit]
            [ HH.text "Join Game"]
          ]
        , HH.p [ css "help is-danger" ]
          [ case st.codeError of
               Nothing -> HH.text ""
               Just NoLobby -> HH.text "Room does not exist."
               Just LobbyFull -> HH.text "Room is full."
          ]
        ]
       ]
        
