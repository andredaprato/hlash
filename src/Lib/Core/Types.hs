{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
module Lib.Core.Types where


import System.Random
import Lib.Core.Profile
import Lib.Effects.RoomCode
import Lib.Db.Users.User (UserId)
import Lib.Core.Question 
import Data.Aeson
import Network.WebSockets (Connection)

myOptions =  defaultOptions {tagSingleConstructors = True,
                              sumEncoding = taggedObject,
                              allNullaryToStringTag = False}
  where taggedObject = TaggedObject
               { tagFieldName      = "tag"
               , contentsFieldName = "contents"
               }

data LobbyError = NoLobby | LobbyFull deriving (Generic, Show, Eq, FromJSON )
instance ToJSON LobbyError where
  toJSON = genericToJSON myOptions
                       
                      



data Lobby = Lobby {
  lobbyCode :: RoomCode,
  lobbyOwner :: Profile,
  lobbyMembers :: [Profile]
  -- lobbyQuestions :: [Question]
  } deriving (Generic, Eq, Show, FromJSON, ToJSON)

data LobbyAction = NewLobby Profile
                 | JoinLobby RoomCode Profile
                 | ExitLobby RoomCode Profile
                 | AddQuestion RoomCode Question
                 | InitGame Lobby deriving (Generic, Show, Eq, ToJSON, FromJSON)

type Lobbies = HashMap RoomCode Lobby
type LobbyStore = TVar Lobbies
type GameStore = TVar Games

data MsgIn = LobbyMsg LobbyAction
           | GameMsg GameMsgIn
           | RequestRoom RoomCode deriving (Generic, Show, ToJSON, FromJSON)

data MsgOut = NewLobbyState Lobby
            | NewGameState Game
            | GameError GameError
            | LobbyError LobbyError deriving (Generic, Show, ToJSON, FromJSON)

 

data GameMsgIn = GameMsgIn GameAction RoomCode deriving (Generic, Eq, Ord, Show, ToJSON)
instance FromJSON GameMsgIn where
  parseJSON = genericParseJSON myOptions

type Index = Int
-- | This seems like a more approriate data type, display the question at the index
data Stage = AwaitingQuestion | Display Index deriving (Generic, Eq, Ord, Show, FromJSON)

instance ToJSON Stage where
  toJSON = genericToJSON myOptions

data Round  = RoundOne | RoundTwo  | RoundThree  | GameOver deriving (Enum, Generic, Eq, Ord, Show, FromJSON)

instance ToJSON Round where
  toJSON = genericToJSON myOptions

data GameState = GameState Round Stage  deriving (Generic, Eq, Ord, Show, FromJSON)
instance ToJSON GameState where
  toJSON = genericToJSON myOptions


data GameAction = Response Question Text
                | Vote Question Profile 
                | ForceNextStage deriving (Generic, Eq, Ord, Show,  FromJSON)
instance ToJSON GameAction where
  toJSON = genericToJSON myOptions

data PlayerAction = PlayerAction {
  player :: Profile,
  action :: GameAction
  } deriving (Generic, Eq, Ord, Show)

data GameError = AlreadyAnswered | NotAssigned |  QuestionError | DoesNotExist deriving (Generic, Eq, Ord, Show,  FromJSON)
instance ToJSON GameError where
  toJSON = genericToJSON myOptions

type Games = HashMap RoomCode Game

data Answer = Answer
  { answer :: Maybe Text
  , score :: Score
  , votedBy :: [Profile]
  } deriving (Generic, Eq, Ord, Show, FromJSON, ToJSON)

emptyAnswer :: Answer
emptyAnswer = Answer Nothing 0 []

type Score = Int
type AssignedQuestions = Map (Question,Profile) Answer

data Game = Game {
  gameCode :: RoomCode,
  members :: [Profile],
  remainingQuestions :: [Question], -- list of all remaining questions to draw from
  currentStageQuestions :: [Question],
  useCount :: Map Question Int,
  answers :: AssignedQuestions,
  membersScore :: Map Profile Score,
  gameState :: GameState
  } deriving (Generic, Eq, Ord, Show, FromJSON) 

displayIndexTimeout :: Int
displayIndexTimeout = 28

awaitingQuestionTimeout :: Int
awaitingQuestionTimeout = 60

instance ToJSON Game where
  toJSON = genericToJSON myOptions
