module Page.Login where


import Data.Newtype
import Prelude

import API.Request (LoginFields)
import Affjax as AX
import Affjax.RequestBody as RB
import Affjax.RequestHeader as RH
import Affjax.ResponseFormat as RF
import Affjax.StatusCode (StatusCode(..))
import Capability.Navigate (class Navigate, navigate)
import Capability.Resource.User (class ManageUser, loginUser)
import Control.Monad.Reader (asks)
import Data.Argonaut.Core (Json, stringify)
import Data.Argonaut.Decode (class DecodeJson, decodeJson)
import Data.Argonaut.Encode (class EncodeJson, encodeJson)
import Data.Const (Const(..))
import Data.Maybe (Maybe(..))
import Data.Route (Route(..))
import Data.Symbol (SProxy(..))
import Data.Username (Username, validUsername)
import Effect.Aff.Class (class MonadAff)
import Effect.Class (liftEffect)
import Formless as F
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import UI.UI (css)
import UI.Form (control, field)
import UI.Form as Form
import UI.Validation (FieldError(..), minLength, toText)
import Web.HTML (window)
import Web.HTML.Location (reload)
import Web.HTML.Window (location)

--- TODO: this component and the Register component are nearly identical

data Action = HandleLoginAction LoginFields  | SignUp

type ChildSlots =  ( formless :: F.Slot LoginForm Query () LoginFields Unit)

component ::  forall m .
              MonadAff m  =>
              ManageUser m =>
              Navigate m => 
              H.Component HH.HTML (Const Void) Unit Unit m
component  = H.mkComponent { initialState : initialState
                           , render
                           , eval : H.mkEval $ H.defaultEval { handleAction = handleAction
                                                             -- , initialize = Just Initialize
                                                             }
                           }
  where
    initialState = const { redirect : false}
    
    render st =
      HH.div [css "section"]
      [ HH.div [css "container"] 
        [
          HH.h1 [css "title", HP.title "Login" ] [ HH.text "Login"]
        , HH.slot F._formless unit formComponent unit (Just <<<  HandleLoginAction)
        , HH.hr_
        , HH.a [ HE.onClick \_ -> Just SignUp ] [ HH.text "Sign up." ]
        ]
      ]
        
    handleAction = case _ of
      HandleLoginAction login -> do
        a <- loginUser login
        case a of
          Nothing -> void $ H.query F._formless unit $ F.injQuery $ CredError true unit 
          Just _ -> H.raise unit *> navigate Home
      SignUp -> navigate Register




newtype LoginForm r f = LoginForm ( r
                                    ( username :: f FieldError  String Username  
                                    , password ::  f FieldError String String ))
derive instance newtypeLoginForm :: Newtype (LoginForm r f) _

data Query a = CredError Boolean a 
derive instance functorQuery :: Functor Query 

formComponent :: forall m . Navigate m => MonadAff m =>  F.Component LoginForm Query () Unit LoginFields m
formComponent = F.component formInput $ F.defaultSpec { render = renderFormless
                                                      , handleEvent = F.raiseResult
                                                      , handleQuery = handleQuery
                                                      }
  where
    formInput _ =   { initialInputs : Nothing
                    , validators : LoginForm { password : F.noValidation
                                             , username : validUsername
                                             }
                    , loginError : false
                    }

    handleQuery :: forall a. Query a -> H.HalogenM _ _ _ _ _  (Maybe a)
    handleQuery = case _ of 
      CredError bool a -> do
        H.modify_ _ { loginError = bool}
        pure (Just a)

    renderFormless st@{form} =
      HH.form_
      [ 
        field [
          HH.label [css "label"]
          [ HH.text "Username" ]
        , control $ Form.input _username form []
        , HH.p [ css "help is-danger" ]
          [ HH.text $ case F.getError _username form of
               Nothing -> ""
               Just error -> toText error 
          ]
        ]
      , field
        [
          HH.label [css "label"] [HH.text "Password"]
        , Form.input _password form [ HP.type_ HP.InputPassword ]
        ]
      , HH.div [ css "field is-grouped"] $
        [
          control $  HH.button
          [ css "button is-link",
            HE.onClick \_ -> Just F.submit,
            HP.type_ HP.ButtonSubmit]
          [ HH.text "Submit"]
          , HH.p [ css "help is-danger" ]
            [ HH.text $ if st.loginError
                        then "Username or password is invalid."
                        else ""
            ]
        ]
      ]

      where _username  = SProxy :: SProxy "username"
            _password = SProxy :: SProxy "password"

