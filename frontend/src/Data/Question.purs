module Data.Question (Question
                     , parse
                     , toString
                     , maxLengthQuestion
                     , nonEmptyQuestion
                     ) where

import Prelude

import Data.Argonaut.Decode (class DecodeJson)
import Data.Argonaut.Encode (class EncodeJson)
import Data.Either (Either(..))
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype)
import Data.String (length)
import Formless as F
import UI.Validation (FieldError(..))

-- | We'll represent usernames as a newtype wrapper around a string. Newtypes have no performance
-- | overhead, so they're the fastest way to add a documenting type to a primitive type like 
-- | `String`.
newtype Question = Question String 

derive instance newtypeQuestion :: Newtype Question _
derive instance genericQuestion :: Generic Question _
derive instance eqQuestion :: Eq Question
derive instance ordQuestion :: Ord Question

-- | We can rely on the `String` JSON instances for encoding and decoding. We don't need to write
-- | them manually.
derive newtype instance encodeJsonQuestion :: EncodeJson Question
derive newtype instance decodeJsonQuestion :: DecodeJson Question

derive newtype instance showQuestion :: Show Question 

-- | This function requires a string to pass some validation before being considered a valid 
-- | `Question`. For now, we'll just enforce a username is non-empty, but we might introduce more
-- | sophisticated validation later on.
parse :: String -> Maybe Question
parse "" = Nothing
parse str = Just (Question  str)

-- | While we don't want to be able to write or manipulate a `Question` without passing validation,
-- | we should still be able to read the string inside. Providing this function makes `Question`
-- | a read-only type.
toString :: Question -> String
toString (Question str) = str



maxLengthQuestion = F.hoistFnE_  \x -> case length x <= 280 of
  false -> Left $ TooLong 280
  true -> Right (Question x)

nonEmptyQuestion = F.hoistFnE_  \x -> case parse x of
  Nothing -> Left $ TooShort 0
  Just a -> Right a
