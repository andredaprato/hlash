{-# LANGUAGE FlexibleContexts #-}
module Hlash.Core.Game.Init where



import qualified Data.Map.Strict as M
import System.Random

import Hlash.Core.Profile
import Hlash.Core.Question 
import Hlash.Core.Types
import Hlash.Core.Game.Utils
import Hlash.Core.Game.Game
import Hlash.Effects.RoomCode
import Hlash.Db (getDefaultQuestions, WithDb)




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

