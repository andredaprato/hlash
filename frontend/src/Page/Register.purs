module Page.Register where

import Prelude

import API.Request (RegisterFields, login)
import Affjax as AX
import Affjax.RequestBody as RB
import Affjax.ResponseFormat as RF
import Affjax.StatusCode (StatusCode(..))
import Capability.Navigate (class Navigate, navigate)
import Capability.Resource.User (class ManageUser, loginUser, registerUser)
import Data.Argonaut (encodeJson)
import Data.Const (Const(..))
import Data.Either (Either(..))
import Data.HTTP.Method (Method(..))
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype)
import Data.Profile (Profile)
import Data.Route (Route(..))
import Data.Symbol (SProxy(..))
import Data.Username (Username, validUsername)
import Effect.Aff (Aff, launchAff_)
import Effect.Aff.Class (class MonadAff)
import Effect.Class.Console (log)
import Formless as F
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import UI.UI (css)
import UI.Form (control, field)
import UI.Form as Form
import UI.Validation (FieldError, minLength, toText)



data Action = HandleRegisterAction RegisterFields
type State = { invalidCredentials :: Boolean}
type ChildSlots =  ( formless :: F.Slot RegisterForm FormQuery () Profile Unit)

component ::  forall m .
              MonadAff m =>
              ManageUser m =>
              Navigate m =>
              H.Component HH.HTML (Const Void) Unit Void m
component  = H.mkComponent { initialState : initialState
                           , render
                           , eval : H.mkEval $ H.defaultEval { handleAction = handleAction
                                                             }
                           }
  where
    initialState = const {invalidCredentials : false}
    
    render st = HH.div [ css "section"] [
     HH.div  [ css "container"]
                                     
                 [ HH.h1 [ css "title", HP.title "Sign up" ] [ HH.text "Sign up"]
                 , HH.slot F._formless unit formComponent unit (Just <<<  HandleRegisterAction)
                 ]
                 
    ]
        
    handleAction = case _ of
      HandleRegisterAction register ->  do
        a <- registerUser register
        case a of
            Nothing -> void $ H.query F._formless unit $ F.injQuery $ CredError true unit
            Just _ -> do void $ H.query F._formless unit $ F.injQuery $ CredError true unit 
                         navigate Home


data FormQuery a = CredError Boolean a 
derive instance functorQuery :: Functor FormQuery 


newtype RegisterForm r f = RegisterForm ( r
                                          (
                                           username :: f FieldError String Username
                                          , password :: f FieldError String String
                                          ))

derive instance newtypeRegisterForm :: Newtype (RegisterForm r f) _
formComponent :: forall m . MonadAff m => F.Component RegisterForm FormQuery () Unit RegisterFields m
formComponent = F.component formInput $ F.defaultSpec {render = renderFormless
                                                              , handleEvent = F.raiseResult
                                                              , handleQuery = handleQuery
                                                              }
  where
    formInput :: Unit -> F.Input RegisterForm (registerError  :: Boolean) m
    formInput _ =   { initialInputs : Nothing
                    , validators : RegisterForm { username : validUsername
                                                , password : minLength 5
                                             }
                    , registerError : false
                    
                  }
    renderFormless st@{ form } =
      HH.form_
      [ 
        field 
        [
          HH.label [css "label"] [HH.text "Username"]
        , control $ Form.input _name form [ HP.type_ HP.InputText ]
        , HH.p [ css "help is-danger" ]
          [ HH.text $ case F.getError _name form of
               Nothing -> ""
               Just error -> toText error 
          ]
        ] 
      , field
        [
          HH.label [css "label"] [HH.text "Password"]
        , control $ Form.input _password form [ HP.type_ HP.InputPassword ]
        , HH.p [ css "help is-danger" ]
          [ HH.text $ case F.getError _password form of
               Nothing -> ""
               Just error -> toText error 
          ]
          ]
        , field 
          [
            HH.button
            [ HP.class_ $ HH.ClassName "button is-link",
              HE.onClick \_ -> Just F.submit,
              HP.type_ HP.ButtonSubmit]
            [ HH.text "Submit"]

          ]
        , HH.p [ css "help is-danger" ]
          [
            case st.registerError of
            true -> HH.text "Username has been taken."
            false -> HH.text ""
           ]
      ]

      where
            _name = SProxy :: _ "username"
            _password = SProxy :: _ "password"

    handleQuery :: forall a. FormQuery a -> H.HalogenM _ _ _ _ _  (Maybe a)
    handleQuery = case _ of 
      CredError bool a -> do
        H.modify_ _ { registerError = bool}
        pure (Just a)
