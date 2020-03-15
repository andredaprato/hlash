{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}
module Lib.Core.Game.Game where

import Prelude hiding (head, tail)

import Control.Monad.ST
import Control.Monad.Random
import System.Random
import Data.Array.ST
import GHC.Arr
import Control.Monad
import Data.List
import qualified Data.Map as M


import Lib.Core.Types
import Lib.Core.Profile
import Lib.Core.Question
import Lib.Core.Game.Utils

type GameM = State Game

runAction :: (StdGen -> Game -> PlayerAction -> Either GameError Game)
runAction gen game pAction = evaluatingState game $ do
  validator <- validateAction pAction
  case validator of
    Nothing -> do runAction' pAction gen
                  st <- get
                  Right <$> pure st
    Just err -> pure $ Left err

validateAction :: PlayerAction -> GameM (Maybe GameError) 
validateAction pAction = case action pAction of
  Response question _ -> isAssignedOrAnswered (question , player pAction)
  _                   -> pure Nothing


runAction' ::  PlayerAction -> StdGen -> GameM ()
runAction' PlayerAction{..} gen = case action of
    Response question str -> updateGameWithAnswer gen question player str
    Vote question prof -> updatePlayerScore question prof player    
    ForceNextStage -> advanceToNextState gen

updateGameWithAnswer :: RandomGen g => g ->  Question -> Profile -> Text -> GameM ()
updateGameWithAnswer gen question player str = 
  modify (\Game{..} ->
    Game { answers = M.insert (question,player) answer answers   
         , useCount = M.update (Just . (+ 1)) question useCount 
         , ..
         }
         )
  >> checkForNextState gen
  where answer = emptyAnswer { answer = Just str }

  -- check that each question in the current stage has been answered by 2 players
checkForNextState :: RandomGen g =>  g -> GameM ()
checkForNextState gen = do
  (currentStageQ, gameS, usedQ, members) <- gets (\Game{..} -> (currentStageQuestions 
                                                               , gameState 
                                                               , useCount
                                                               , members ))
  -- every question must be answered twice in the first two rounds to advance stages,
  -- or by every member if we are in the final stage
  let answered = and $ checkAnswered members gameS usedQ <$> currentStageQ  

  bool (pure ()) (advanceToNextState gen)  answered

    where checkAnswered members gameState usedQ q =
            let n = case gameState of
                  GameState RoundThree _ -> length members
                  _ ->  2
            in  fullCount usedQ q n

          fullCount usedQ q n = case  M.lookup q usedQ of
            Nothing -> False
            Just ct -> ct == n

advanceToNextState :: RandomGen g => g -> GameM ()
advanceToNextState gen = 
  modify (\g@Game{..} -> case getRound (getNextState g) == getRound gameState of

             False -> case getNextState g of
               GameState GameOver _ -> Game { currentStageQuestions = takeNextQuestions g
                                            , remainingQuestions = drop (numQuestions g) remainingQuestions
                                            , gameState = getNextState g 
                                            , ..
                                            }
               nextRound            ->  Game { currentStageQuestions = takeNextQuestions g
                                             , remainingQuestions = drop (numQuestions g) remainingQuestions
                                             , gameState = getNextState  g
                                             , answers = M.union answers $
                                               generatePlayerQuestions gen (getRound nextRound) (takeNextQuestions g)   members
                                             , ..
                                             }

             True  -> Game { gameState = getNextState  g
                           , ..
                           } 
         )
    where
      takeNextQuestions g@Game{..} = take (numQuestions g) remainingQuestions
      numQuestions g = questionsPerRound $ members g

      getNextState  g@Game{..} = case gameState of
        GameState round (Display ix) -> advanceIndex round ix g
        GameState round AwaitingQuestion -> GameState round (Display 0)

      advanceIndex round ix Game{..} = if ix == length currentStageQuestions - 1 
        then GameState (succ round) AwaitingQuestion
        else GameState round (Display $ succ ix)
        
-- | check for double update in validation, and add full quiplash scoring (winner gets bonus, quiplash gets bonus)
updatePlayerScore :: Question -> Profile -> Profile -> GameM ()
updatePlayerScore question votedFor player =
  modify (\Game{..} ->
             Game { membersScore =
                      M.update (Just . (+ 100)) votedFor membersScore
                  , answers =
                      M.update (\ans -> Just $ ans { score = score ans + 100
                                                   , votedBy = player :  votedBy ans
                                                 }
                               ) (question, votedFor) answers
                  , ..
                  }
         )
 
questionsPerRound  :: [Profile] -> Int
questionsPerRound  = length 
  

getRound :: GameState -> Round 
getRound (GameState round _  ) = round

isAssignedOrAnswered :: (Question, Profile) -> GameM (Maybe GameError) 
isAssignedOrAnswered k = do
  ans <- gets answers
  pure $ case M.lookup k  ans of
    Nothing -> Just NotAssigned
    Just mAns -> maybe Nothing (const $ Just AlreadyAnswered) $ answer mAns

  

