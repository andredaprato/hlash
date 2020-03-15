module Lib.Page.DisplayAnswers where

import Prelude

import Capability.Resource.Game (class ManageGame, voteForQuestion)
import Component.Utils (timerEventSource)
import Data.Array (filter, head, null, tail)
import Data.Const (Const(..))
import Data.Game (AssignedQuestion, Game, isAssigned)
import Data.Map (Map, empty, update)
import Data.Maybe (Maybe(..), isJust, maybe)
import Data.Profile (Profile)
import Data.Question (Question, toString)
import Data.RoomCode (RoomCode(..))
import Data.Tuple (Tuple(..))
import Data.Username as U
import Effect.Aff.Class (class MonadAff)
import Effect.Class.Console (log, logShow)
import Formless as F
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Page.DisplayMembers (renderMembers)
import Socket.Types (MsgIn(..))
import UI.UI (column, css)
import UI.Form as Form
import UI.Validation (FieldError(..), maxLength, toText)

type InputRep = ( question   :: Question
                , answers     :: Array AssignedQuestion
                , currentUser :: Profile
                , gameCode    :: RoomCode
                )

data Voting = Off | Vote | DisplayVotes
data RenderAnswers = OnlyQuestion | DisplayAnswers

type State = { voting :: Voting
             , renderAnswers :: RenderAnswers
             , voted :: Boolean
             , sid :: Maybe H.SubscriptionId
               | InputRep
             }
type Input = { | InputRep}
data Action = Receive Input 
            | Initialize
            | ToggleNext Unit
            | VoteForQuestion AssignedQuestion
              
component ::  forall m .
              MonadAff m  =>
              ManageGame m =>
              H.Component HH.HTML (Const Void) Input Int m
component  = H.mkComponent { initialState : initialState
                           , render
                           , eval : H.mkEval $ H.defaultEval { initialize = Just Initialize
                                                             , handleAction = handleAction
                                                             , receive = Just <<< Receive
                                                             }
                           }
  where initialState = \ {question, answers, currentUser, gameCode} -> { voting : Off
                                                                       , renderAnswers : OnlyQuestion
                                                                       , question  : question
                                                                       , answers : answers
                                                                       , currentUser : currentUser
                                                                       , voted : false
                                                                       , gameCode : gameCode
                                                                       , sid : Nothing
                                                                       }
        handleAction :: Action -> H.HalogenM State _ _ _ m Unit
        handleAction = case _ of
          Receive input -> do
            st <- H.get
            if input.question == st.question 
              then H.modify_ \st -> st { answers = input.answers }
              else do  H.modify_ \st -> st { question = input.question
                                           , answers = input.answers
                                           , gameCode = input.gameCode
                                           , voting = Off
                                           , renderAnswers = OnlyQuestion
                                           , voted = false
                                           , sid = Nothing
                                           }
                       handleAction Initialize
            

          Initialize -> do
            sid <- H.subscribe (ToggleNext <$> timerEventSource 4.0)
            H.modify_ _ { sid = Just sid }
            H.raise 4
            pure unit

          ToggleNext a -> do
            st <- H.get
            case st.renderAnswers of
              OnlyQuestion -> do
                H.modify_ _ { renderAnswers = DisplayAnswers
                            }
                H.raise 4

              DisplayAnswers -> case  st.voting of
                Off -> do
                  logShow $ isJust st.sid
                  maybe (pure unit)  H.unsubscribe  st.sid
                  sid <- H.subscribe (ToggleNext <$> timerEventSource 10.0)
                  H.modify_ _ {voting = Vote, sid = Just sid} *> H.raise 10

                Vote -> do
                  H.modify_ _ { voting = DisplayVotes }
                  maybe (pure unit) H.unsubscribe  st.sid
                  H.raise 10

                DisplayVotes -> pure unit

          VoteForQuestion assignedQuestion -> do
             -- disable buttons, send the vote to the backend
            st <- H.get
            voteForQuestion assignedQuestion.question assignedQuestion.member st.gameCode
            H.modify_ _ { voted = true }

        render st =
          HH.div [ css "container"]
          [
            HH.div [ css "columns is-centered is-multiline "] $
            [
              column "is-full"
              [
                HH.div [ css "notification is-info"]
                [
                  HH.p [ css " is-size-4 has-text-centered "]
                  [
                    HH.text $ toString st.question
                  ]
                ]
              , HH.hr [ css "has-background-info"]

              , HH.h1 [ css "subtitle is-size-6 has-text-centered"] [ HH.text "Responses " ]
                
              ]
            ] <>  (renderAssignedQuestion st $ filter (\a -> a.question == st.question ) st.answers)

          ]

        renderAssignedQuestion st answers =
          case st.renderAnswers of
            OnlyQuestion -> [HH.text ""]
            DisplayAnswers -> let allowVoting =  isAssigned st.currentUser answers
                              in  map (renderAnswer st allowVoting) $ answers  --TODO: check that this answer isn't the currentUser
                              

        renderAnswer st isAssigned assignedQuestion =
          HH.div [ css "column" ]
          [
            HH.div [ css "box"]
            [
              if displayingVotes st.voting
              then HH.div [ css "tag is-centered is-primary" ]
                      [
                        HH.text $ U.toString assignedQuestion.member.username
                      ]
              else HH.text ""

            ,  HH.h1 [css "title bold has-text-centered"]
               [
                 renderMaybe assignedQuestion.answer
               ]

            , case st.voting of
                 Vote ->
                   if isAssigned
                   then HH.text ""
                    else HH.button [ css "button is-primary"
                              , HP.disabled st.voted
                              , HE.onClick \_ -> Just $ VoteForQuestion assignedQuestion ]
                            [
                              HH.text "Vote for: ", renderMaybe  assignedQuestion.answer
                            ]

                 Off -> HH.text ""
                 DisplayVotes ->  renderVotes assignedQuestion

            ]
          ]
        renderVotes assq = HH.div [ css "columns is-vcentered"] $  
                      [
                        column "is-7"
                        [
                          if null  assq.votedBy
                          then HH.text ""
                          else  renderMembers $ Just <$> assq.votedBy
                        ]
                      , column ""
                        [
                          HH.div [ css "notification is-primary" ]
                          [
                            HH.h1 [ css "subtitle has-text-centered" ]
                            [
                              HH.text "Points"
                            ]
                          , HH.br_
                          , HH.p [ css "has-text-centered is-size-1"]
                            [
                              HH.text $ show assq.score 
                            ]
                          ]
                        ]
                      ]
        
        renderMaybe = case _ of
          Nothing -> HH.text ""
          Just a -> HH.p [ css "has-text-centered"] [ HH.text  a]

        displayingVotes DisplayVotes = true
        displayingVotes other = false
        
