module Data.Profile where

import Data.Argonaut.Decode.Generic.Rep
import Data.Argonaut.Encode.Generic.Rep
import Data.Question
import Data.Username
import Prelude

import Data.Argonaut.Decode (class DecodeJson)
import Data.Argonaut.Encode (class EncodeJson)
import Data.Argonaut.Types.Generic.Rep (defaultEncoding)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Maybe (Maybe(..))
import Data.RoomCode (RoomCode)


type ProfileRep row =
  ( username :: Username
  | row
)
type Profile =
  { | ProfileRep () }


data LobbyAction = NewLobby Profile
                 | JoinLobby RoomCode Profile
                 | ExitLobby RoomCode Profile 



derive instance genericLobbyAction :: Generic LobbyAction _
derive instance eqLobbyAction :: Eq LobbyAction
derive instance ordLobbyAction :: Ord LobbyAction
instance showLobbyAction :: Show LobbyAction where
  show = genericShow
instance encodeJsonLobbyAction :: EncodeJson LobbyAction where
  encodeJson = genericEncodeJsonWith $ defaultEncoding {valuesKey = "contents"
                                                       , unwrapSingleArguments = true}
instance decodeJsonLobbyAction :: DecodeJson LobbyAction where
  decodeJson = genericDecodeJson
