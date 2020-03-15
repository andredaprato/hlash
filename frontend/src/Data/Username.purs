module Data.Username ( Username
                     , parse
                     , toString
                     , validUsername
                ) where

import Prelude

import Data.Argonaut.Decode (class DecodeJson)
import Data.Argonaut.Encode (class EncodeJson)
import Data.Either (Either(..))
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype, unwrap)
import Data.String (length)
import Formless as F
import UI.Validation (FieldError(..))

-- | We'll represent usernames as a newtype wrapper around a string. Newtypes have no performance
-- | overhead, so they're the fastest way to add a documenting type to a primitive type like 
-- | `String`.
newtype Username = Username String

derive instance genericUsername :: Generic Username _
derive instance eqUsername :: Eq Username
derive instance ordUsername :: Ord Username
derive instance newtypeUsername :: Newtype Username _

-- | We can rely on the `String` JSON instances for encoding and decoding. We don't need to write
-- | them manually.
derive newtype instance encodeJsonUsername :: EncodeJson Username
derive newtype instance decodeJsonUsername :: DecodeJson Username

derive newtype instance showUsername :: Show Username 

-- | This function requires a string to pass some validation before being considered a valid 
-- | `Username`. For now, we'll just enforce a username is non-empty, but we might introduce more
-- | sophisticated validation later on.
parse :: String -> Maybe Username
parse "" = Nothing
parse str = Just (Username str)

-- | While we don't want to be able to write or manipulate a `Username` without passing validation,
-- | we should still be able to read the string inside. Providing this function makes `Username`
-- | a read-only type.
toString :: Username -> String
toString (Username str) = str

validUsername = F.hoistFnE_  \x -> case length x < 5 of
                            true -> Left $ TooShort 5
                            false -> Right (Username x)
