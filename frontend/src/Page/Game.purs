module Page.Game where


import Prelude

import Capability.Resource.Game (class ManageGame, submitAnswer)
import Component.Timer as Timer
import Data.Array (head, tail, (!!))
import Data.Const (Const(..))
import Data.Foldable (elem)
import Data.Game (Game, GameState(..), Round(..), Stage(..), checkRoundChanged, gameIsOver, getCurrentAnswers, getPlayerQuestions)
import Data.Map as M
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype)
import Data.Profile (Profile)
import Data.Question (Question, toString)
import Data.RoomCode (removeRoomCode)
import Data.Set as S
import Data.Symbol (SProxy(..))
import Data.Tuple (fst, snd)
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
import Socket.Types (MsgIn(..))
import UI.UI (column, css)
import UI.Form (control, field)
import UI.Form as Form
import UI.Validation (FieldError(..), maxLength, toText)

type State =
  {
    loading :: Boolean
  , currentUser :: Profile
  , game :: Game
  , playerQuestions :: Array Question
  , currentQuestion :: Maybe Question
  , timer :: Int
  }

data Query a = Action MsgIn a
data Action = Initialize | SubmitAnswer Answer | UpdateDuration Int
type ChildSlots = ( timer :: H.Slot (Const Void) Int Unit)

component ::  forall m .
              MonadAff m  =>
              ManageGame m =>
              H.Component HH.HTML Query {currentUser :: Profile, game :: Game} Void m
component  = H.mkComponent { initialState : initialState
                           , render
                           , eval : H.mkEval $ H.defaultEval { initialize = Just Initialize
                                                             , handleAction = handleAction
                                                             , handleQuery = handleQuery}
                           }
  where
    initialState = \ {currentUser, game} -> { loading : false
                                            , game : game
                                            , currentUser: currentUser
                                            , playerQuestions : []
                                            , currentQuestion : Nothing
                                            , timer : 60 
                                              }
    handleAction  = case _ of
      Initialize -> do
        st <- H.get 

        modifyNextQuestion $ getPlayerQuestions st.currentUser st.game
        pure unit

      SubmitAnswer ans ->  do
        st <- H.get
        case st.currentQuestion of
          Nothing -> pure unit
          Just q -> do
            submitAnswer q ans.answer st.game.gameCode
            modifyNextQuestion st.playerQuestions

      UpdateDuration time -> H.modify_ _ {timer = time}

    handleQuery :: forall a. Query a -> H.HalogenM State Action _ _ m (Maybe a)
    handleQuery (Action msg a) = case msg of
      NewGameState newGame -> do
        st <- H.get
        when (checkRoundChanged st.game newGame) $ do
          modifyNextQuestion $ getPlayerQuestions st.currentUser newGame
          H.modify_ _ { timer = 60 }
           
        when (gameIsOver newGame.gameState) $ H.liftEffect removeRoomCode 
        H.modify_ _ {game = newGame} 
        pure (Just a)

      _ -> pure (Just a)
    
    render st =
      HH.div [ css "section has-background-light" ]
      [ 
        HH.div [ css "columns" ]
        [
          column "is-9"
          [
            displayGame st
          ]
        , HH.div [ css "columns is-multiline is-centered" ]
            [
              column "is-full"
              [
                renderMembers $ map Just st.game.members
              ]
            , column "has-text-centered"
              [
                HH.slot (SProxy :: _ "timer") unit Timer.component st.timer (Just <<< UpdateDuration)
              ]
            ]
        ]
      ]  

    displayGame st = 
      case st.game.gameState of
        GameState GameOver _ ->
          renderGameOver st.game.membersScore

        GameState _ AwaitingQuestion ->
          case st.currentQuestion of
          Nothing -> HH.h1 [ css "title has-text-centered "]
                     [
                       HH.text "Waiting for other players answers"
                     ]

          Just q -> renderCurrentQuestion q

        GameState _ (Display ix) ->
          case st.game.currentStageQuestions !! ix of
            Nothing -> HH.text "error"

            Just currentQuestion ->
              HH.slot (SProxy :: _ "answerDisplay") unit DisplayAnswers.component displayAnswersInput (Just <<< UpdateDuration) 

              where displayAnswersInput = { question : currentQuestion
                                         , answers : getCurrentAnswers st.game currentQuestion
                                         , currentUser : st.currentUser
                                         , gameCode : st.game.gameCode}

                                    

    modifyNextQuestion pQuestions = H.modify_ _ { playerQuestions = case tail pQuestions of
                                                     Nothing -> []
                                                     Just xs -> xs
                                                , currentQuestion = head pQuestions
                                                }

    renderCurrentQuestion q = HH.slot F._formless unit formComponent q (Just <<< SubmitAnswer)

type Answer = { answer :: String  }
newtype AnswerForm r f  = AnswerForm ( r
                                       ( answer :: f FieldError String String
                                       )
                                     )
derive instance newtypeAnswerForm :: Newtype (AnswerForm r f) _

prx = F.mkSProxies (F.FormProxy :: _ AnswerForm)
data FormAction  = Receive Question
formComponent :: forall m . MonadAff m => F.Component AnswerForm Query () Question Answer m
formComponent = F.component formInput $ F.defaultSpec { render = renderFormless
                                                      , handleEvent = F.raiseResult
                                                      , receive = Just <<< Receive
                                                      , handleAction = handleAction
                                                               }
  where
    formInput q = { initialInputs : Nothing
                  , question : q
                  , validators : AnswerForm { answer : maxLength 20
                                            }
                  }
    renderFormless st@{form} =
      HH.div [ css "container"] 
      [

       HH.form_
       [
         field [
           HH.div [ css "columns is-centered"]

           [ column "is-half"
             [
               HH.div [ css "notification is-info"]
               [
                 HH.p [ css " is-size-4 has-text-centered "]
                 [
                   HH.text $ toString st.question
                 ]
               ]
             ]
           ]
         ,  control $ Form.input prx.answer form [ css "input is-info is-large"
                                                 , HP.placeholder "Enter something clever"]

         ]
       , field $
         [
           control $ HH.button
           [ css "button is-info is-centered is-medium",
             HE.onClick \_ -> Just F.submit,
             HP.type_ HP.ButtonSubmit]
           [ HH.text "Submit"]
         ]

       , HH.text case F.getError prx.answer form of
           Nothing -> ""
           Just a -> toText a

       ]
      ]

    handleAction ::  FormAction -> H.HalogenM _ _ _ _ m Unit 
    handleAction = case _ of
      Receive question -> H.modify_ _ { question = question
                                      }
