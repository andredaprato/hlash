module Socket.Types where

import Data.Argonaut (class DecodeJson, class EncodeJson)
import Data.Argonaut.Decode.Generic.Rep (genericDecodeJson, genericDecodeJsonWith)
import Data.Argonaut.Types.Generic.Rep (defaultEncoding)
import Data.Game (Game(..), GameError, GameMsgIn(..))
import Data.Generic.Rep (class Generic)
import Data.Lobby (Lobby, LobbyAction, LobbyError)
import Data.RoomCode (RoomCode)
import Data.Utils (myGenericDecode, myGenericEncode)


data MsgOut = LobbyMsg LobbyAction | GameMsg GameMsgIn | RequestRoom RoomCode

data MsgIn = NewLobbyState Lobby
           | NewGameState Game
           | GameError GameError
           | LobbyError LobbyError

derive instance genericMsgOut :: Generic MsgOut _
instance encodeJsonMsgOut :: EncodeJson MsgOut where
  encodeJson = myGenericEncode
instance decodeJsonMsgOut :: DecodeJson MsgOut where
  decodeJson = genericDecodeJson

derive instance genericMsgIn :: Generic MsgIn _
instance encodeJsonMsgIn :: EncodeJson MsgIn where
  encodeJson = myGenericEncode
instance decodeJsonMsgIn :: DecodeJson MsgIn where
  decodeJson = myGenericDecode
