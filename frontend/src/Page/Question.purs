module Page.Question where

import Prelude

import Affjax as AX
import Affjax.RequestBody as RB
import Affjax.RequestHeader as RH
import Affjax.ResponseFormat as RF
import Affjax.StatusCode (StatusCode(..))
import Capability.Navigate (class Navigate, navigate)
import Capability.Resource.Lobby (class ManageLobby)
import Capability.Resource.Question (class ManageQuestion, getQuestions, makeQuestion)
import Data.Argonaut.Core (Json, stringify)
import Data.Argonaut.Decode (class DecodeJson, decodeJson)
import Data.Argonaut.Encode (class EncodeJson, encodeJson)
import Data.Bifunctor (bimap)
import Data.Const (Const(..))
import Data.Either (Either(..), either)
import Data.Functor as Functor
import Data.Generic.Rep (class Generic)
import Data.HTTP.Method (Method(..))
import Data.Int (fromString)
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype, unwrap)
import Data.Question (Question, maxLengthQuestion, nonEmptyQuestion)
import Data.Route (Route(..))
import Data.Symbol (SProxy(..))
import Effect.Aff (Aff, launchAff, launchAff_)
import Effect.Aff.Class (class MonadAff)
import Effect.Class.Console (log, logShow)
import Formless as F
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.Query as HQ
import UI.UI (css)
import UI.Form (control, field)
import UI.Form as Form
import UI.Validation (FieldError(..), minLength, toText)
import Web.Event.Event as Event


------------------------ separate files

data Action = HandleQuestionAction QuestionFields  | SetCurrentQuestions
type ChildSlots =  ( formless :: F.Slot QuestionForm Query () Question Unit )
type State = { invalidCredentials :: Boolean
             , questions :: Array Question}


component ::  forall m .
              MonadAff m =>
              ManageQuestion m =>
              ManageLobby m =>
              H.Component HH.HTML (Const Void) Unit Void m
component  = H.mkComponent { initialState : initialState
                           , render
                           , eval : H.mkEval $ H.defaultEval { initialize = Just SetCurrentQuestions
                                                             , handleAction = handleAction}
                           }
  where
    initialState = const { invalidCredentials : false, questions : [] }
    
    render st =
      HH.div [ css "container"]
      [
        HH.h1 [ css "title" ] [ HH.text "Create your own question"]
      , HH.hr_
      , HH.slot F._formless unit formComponent unit (Just <<<  HandleQuestionAction)
        
      , HH.div [ css "section"]
        [
          HH.table [css "table is-fullwidth is-bordered is-striped"]
          [
            HH.thead_
            [
              HH.tr_
              [
                HH.th [ css "has-text-centered"]
                [ HH.text "Your Questions" ]
              ] 
            ]
          , HH.tbody_ $
            
            map  (\q -> HH.tr [ ]
                        [
                          HH.th_ [HH.text $ unwrap q ]
                        ]
                 ) st.questions
            
            ]
          ]
      ]
        
    handleAction = case _ of
      HandleQuestionAction question ->  do
        makeQuestion question.questionBody *> handleAction SetCurrentQuestions
      SetCurrentQuestions -> do
        getQuestions  >>= case _ of
          Nothing -> pure unit
          Just questions -> H.modify_ _ { questions = questions }
        
              

type QuestionFields = {questionBody :: Question}

newtype QuestionForm r f = QuestionForm ( r
                                          ( questionBody :: f FieldError String Question  
                                          ))
derive instance newtypeQuestionForm :: Newtype (QuestionForm r f) _

data Query a = CredError Boolean a 
derive instance functorQuery :: Functor Query 

formComponent :: forall m . MonadAff m => F.Component QuestionForm Query () Unit QuestionFields m
formComponent = F.component formInput $ F.defaultSpec { render = renderFormless
                                                      , handleEvent = F.raiseResult
                                                              }
  where
    formInput :: Unit -> F.Input QuestionForm _ m
    formInput _ =   { initialInputs : Nothing
                    , validators : QuestionForm { questionBody : nonEmptyQuestion 
                                                }
                    
                  }
    renderFormless st@{form} =
      HH.form_
      [ 
        field [
          HH.label [HP.class_ $ HH.ClassName "field"] [HH.text "Question"]

        , control $  Form.input _question form []
        ]

      , field  [
           control $  HH.button
            [ HP.class_ $ HH.ClassName "button is-link",
              HE.onClick \_ -> Just F.submit,
              HP.type_ HP.ButtonSubmit]
            [ HH.text "Submit"]
          ]

      ]

      where _question  = SProxy :: SProxy "questionBody"
