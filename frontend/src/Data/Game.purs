module Data.Game where

import Data.Lobby
import Data.Question
import Prelude

import Data.Argonaut (class DecodeJson, class EncodeJson)
import Data.Array (elem, filter, length, toUnfoldable)
import Data.Generic.Rep (class Generic)
import Data.Map (Map)
import Data.Map as M
import Data.Maybe (Maybe)
import Data.Profile (Profile)
import Data.RoomCode (RoomCode(..))
import Data.Set as S
import Data.Tuple (Tuple(..), fst, snd)
import Data.Utils (myGenericDecode, myGenericEncode)


-- this is the reverse (that is we output these types to the WS),
-- just for server communication
data GameMsgIn = GameMsgIn GameAction RoomCode

type Index = Int

data Stage = AwaitingQuestion | Display Index

data Round = RoundOne | RoundTwo | RoundThree | GameOver

data GameState = GameState Round Stage

data GameAction = Response Question String
                | Vote Question Profile 
                | ForceNextStage


                  

data GameError = AlreadyAnswered | QuestionError | DoesNotExist

type Score = Int

type AnswerRep r = ( answer :: Maybe String
                   , score :: Score
                   , votedBy :: Array Profile
                     | r
                   )
type Answer = { |  AnswerRep () } 

type AssignedQuestion = { question :: Question
                        , member :: Profile
                        | AnswerRep () 
                        }
-- big TODO: i'd really prefer to change this to not depend on the server representation of the game
-- and instead just receive server messages that are directly renderable

type Game = {
  gameCode :: RoomCode,
  members :: Array Profile,
  remainingQuestions :: Array Question, 
  currentStageQuestions :: Array Question,
  useCount  :: Map Question Int,
  answers :: Map (Tuple Question Profile) Answer,

  membersScore :: Map Profile Score,
  gameState :: GameState
  }

derive instance genericGameMsgIn :: Generic GameMsgIn _
instance encodeJsonGameMsgIn :: EncodeJson GameMsgIn where
  encodeJson = myGenericEncode
instance decodeJsonGameMsgIn :: DecodeJson GameMsgIn where
  decodeJson = myGenericDecode

derive instance genericGameAction :: Generic GameAction _
instance encodeJsonGameAction :: EncodeJson GameAction where
  encodeJson = myGenericEncode
instance decodeJsonGameAction :: DecodeJson GameAction where
  decodeJson = myGenericDecode

derive instance genericStage :: Generic Stage _
instance encodeJsonStage :: EncodeJson Stage where
  encodeJson = myGenericEncode
instance decodeJsonStage :: DecodeJson Stage where
  decodeJson = myGenericDecode

derive instance genericRound :: Generic Round _
derive instance eqRound :: Eq Round 
instance encodeJsonRound :: EncodeJson Round where
  encodeJson = myGenericEncode
instance decodeJsonRound :: DecodeJson Round where
  decodeJson = myGenericDecode
derive instance genericGameState :: Generic GameState _
instance encodeJsonGameState :: EncodeJson GameState where
  encodeJson = myGenericEncode
instance decodeJsonGameState :: DecodeJson GameState where
  decodeJson = myGenericDecode

derive instance genericGameError :: Generic GameError _
instance encodeJsonGameError :: EncodeJson GameError where
  encodeJson = myGenericEncode
instance decodeJsonGameError :: DecodeJson GameError where
  decodeJson = myGenericDecode

checkRoundChanged game newGame = getRound game.gameState  /= getRound newGame.gameState
  
getRound (GameState round _) = round

isAssigned :: Profile -> Array AssignedQuestion  -> Boolean
isAssigned prof assq = let x = filter (\q -> q.member == prof) assq
                       in length x > 0

  
gameIsOver :: GameState -> Boolean
gameIsOver (GameState GameOver _) = true
gameIsOver otherwise = false

getCurrentAnswers :: Game -> Question -> Array AssignedQuestion
getCurrentAnswers game question = map toAssignedQuestion
                                  $ M.toUnfoldable
                                  $ M.filterKeys (\x -> fst x == question
                                                 ) game.answers

  where toAssignedQuestion (Tuple (Tuple q p) a) = { question: q
                                                   , member:  p
                                                   , answer :  a.answer
                                                   , score : a.score
                                                   , votedBy : a.votedBy
                                                   }

getPlayerQuestions :: Profile -> Game -> Array Question
getPlayerQuestions player game =  S.toUnfoldable $ S.map fst $ M.keys $ M.filterKeys (\x ->  snd x == player &&
                                                                                         fst x `elem` game.currentStageQuestions
                                                                                 ) game.answers
