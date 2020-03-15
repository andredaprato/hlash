module UI.Validation where

import Prelude

import Data.Argonaut.Decode (class DecodeJson)
import Data.Argonaut.Decode.Generic.Rep (genericDecodeJson)
import Data.Argonaut.Encode (class EncodeJson, encodeJson)
import Data.Either (Either(..))
import Data.Foldable (length) as Foldable
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Lens (preview)
import Data.Maybe (Maybe)
import Data.Newtype (class Newtype)
import Data.String (contains, length)
import Data.String.Pattern (Pattern(..))
import Effect.Aff.Class (class MonadAff)
import Formless as F



data FieldError = TooLong Int | TooShort Int | InvalidEmail | EmptyField | InvalidCode

derive instance genericFieldError :: Generic FieldError _
instance showFieldError :: Show FieldError where
  show = genericShow

instance toTextFieldError :: ToText FieldError where
  toText (TooLong n) = "Field must be " <>  show n <> " characters or less."
  toText  (TooShort n) = "Field must be " <>  show n <> " characters or more."
  toText InvalidEmail = "Email format is invalid."
  toText EmptyField = "Field must be filled."
  toText InvalidCode = "Invalid room code format."
  
class ToText item where
  toText :: item -> String

minLength :: ∀ form m. Monad m => Int -> F.Validation form m FieldError String String
minLength n = F.hoistFnE_ $ \str ->
  let n' = length str
   in if n' < n then Left (TooShort n) else Right str

maxLength :: ∀ form m. Monad m => Int -> F.Validation form m FieldError String String
maxLength n = F.hoistFnE_ $ \str ->
  let n' = length str
   in if n' > n then Left (TooLong n) else Right str

         -- | Unpacks errors to render as a string
showError :: forall e o. ToText e => F.FormFieldResult e o -> Maybe String
showError = map toText <<< preview F._Error
