module Data.Lobby where

import Data.Profile
import Prelude

import Data.Argonaut (class DecodeJson, class EncodeJson, decodeJson)
import Data.Argonaut.Decode (getField, (.:))
import Data.Argonaut.Decode.Generic.Rep (genericDecodeJson)
import Data.Argonaut.Encode.Generic.Rep (genericEncodeJsonWith)
import Data.Argonaut.Types.Generic.Rep (defaultEncoding)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Newtype (class Newtype)
import Data.Utils (myGenericEncode)
import Data.RoomCode (RoomCode)

type Lobby =  { lobbyCode :: RoomCode
              , lobbyOwner :: Profile
              , lobbyMembers :: Array Profile
                   } 


data LobbyAction = NewLobby Profile
                 | JoinLobby RoomCode Profile
                 | ExitLobby RoomCode Profile 
                 | InitGame Lobby
data LobbyError = NoLobby | LobbyFull


derive instance genericLobbyAction :: Generic LobbyAction _
derive instance eqLobbyAction :: Eq LobbyAction
derive instance ordLobbyAction :: Ord LobbyAction
instance showLobbyAction :: Show LobbyAction where
  show = genericShow
instance encodeJsonLobbyAction :: EncodeJson LobbyAction where
  encodeJson = myGenericEncode
instance decodeJsonLobbyAction :: DecodeJson LobbyAction where
  decodeJson = genericDecodeJson


derive instance genericLobbyError :: Generic LobbyError _
derive instance eqLobbyError :: Eq LobbyError
derive instance ordLobbyError :: Ord LobbyError
instance showLobbyError :: Show LobbyError where
  show = genericShow
instance encodeJsonLobbyError :: EncodeJson LobbyError where
  encodeJson = myGenericEncode
instance decodeJsonLobbyError :: DecodeJson LobbyError where
  decodeJson = genericDecodeJson
    

