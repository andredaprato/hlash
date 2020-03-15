{-# LANGUAGE FlexibleContexts #-}
module Lib.Core.Game.Init where



import qualified Data.Map.Strict as M
import System.Random

import Lib.Core.Profile
import Lib.Core.Question 
import Lib.Core.Types
import Lib.Core.Game.Utils
import Lib.Core.Game.Game
import Lib.Effects.RoomCode
import Lib.Db (getDefaultQuestions, WithDb)




initGame :: (WithDb env m , RandomGen g) => RoomCode -> [Profile] -> g -> m Game
initGame roomCode members g = do
  randomQuestions <- Question <<$>> getDefaultQuestions (numQuestionsPerRound * 2 + 1 )
  let currentStageQuestions = take numQuestionsPerRound randomQuestions 

  pure $ Game {
    gameCode = roomCode,
    members = members,
    remainingQuestions = drop numQuestionsPerRound randomQuestions ,
    currentStageQuestions = currentStageQuestions,
    useCount = M.fromList $ zip randomQuestions (repeat 0),
    answers = generatePlayerQuestions g RoundOne currentStageQuestions members,
    membersScore = M.fromList $ zip members (repeat 0) ,
    gameState = GameState RoundOne AwaitingQuestion
  }
  
  where numQuestionsPerRound = questionsPerRound members

