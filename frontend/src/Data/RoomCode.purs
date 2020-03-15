module Data.RoomCode ( RoomCode,
                       toUpper,
                       toString,
                       retreiveRoomCode,
                       writeRoomCode,
                       validateRoomCode,
                       removeRoomCode
                       ) where

import Prelude

import Data.Argonaut (class DecodeJson, class EncodeJson, decodeJson, stringify)
import Data.Char.Unicode (isAlpha)
import Data.Either (Either(..))
import Data.Foldable (all)
import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe)
import Data.Newtype (class Newtype)
import Data.String (length)
import Data.String as S
import Data.String.CodeUnits (toCharArray)
import Effect (Effect)
import Formless.Validation (hoistFnE_)
import Formless.Validation as F
import UI.Validation (FieldError(..))
import Web.HTML (window)
import Web.HTML.Window (localStorage)
import Web.Storage.Storage (getItem, removeItem, setItem)

newtype RoomCode = RoomCode String

derive instance genericRoomCode :: Generic RoomCode _
derive instance eqRoomCode :: Eq RoomCode
derive instance ordRoomCode :: Ord RoomCode
derive newtype instance showJsonRoomCode :: Show RoomCode
derive newtype instance encodeJsonRoomCode :: EncodeJson RoomCode
derive newtype instance decodeJsonRoomCode :: DecodeJson RoomCode
derive instance newtypeRoomCode :: Newtype RoomCode _



toUpper :: RoomCode -> RoomCode
toUpper (RoomCode code) = RoomCode $ S.toUpper code

toString :: RoomCode  -> String
toString (RoomCode code) = code

roomCodeKey = "roomCode" :: String

retreiveRoomCode :: Effect (Maybe RoomCode)
retreiveRoomCode = do 
  str <- getItem roomCodeKey =<< localStorage =<< window
  pure $ RoomCode <$> str

writeRoomCode :: RoomCode -> Effect Unit
writeRoomCode (RoomCode code) = setItem roomCodeKey code =<< localStorage =<< window

removeRoomCode :: Effect Unit
removeRoomCode = removeItem roomCodeKey =<< localStorage =<< window

validateRoomCode :: ∀ form m. Monad m =>  F.Validation form m FieldError String RoomCode
validateRoomCode = hoistFnE_  \str -> 
  case all isAlpha (toCharArray str) && length str == 4 of
    true -> Right $ RoomCode  str
    false -> Left $ InvalidCode
