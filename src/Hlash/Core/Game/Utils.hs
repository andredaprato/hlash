module Hlash.Core.Game.Utils where

import Prelude hiding (head, tail)

import Control.Monad.ST
import Control.Monad.Random
import System.Random
import Data.Array.ST
import GHC.Arr
import qualified Data.Map as M
import Data.List

import Hlash.Core.Profile
import Hlash.Core.Question
import Hlash.Core.Types

  -- | Generate the assigned questions for each player in a round of the game,
  -- note that this function guarantees each player is assigned exactly two questions.
-- generatePlayerQuestions :: RandomGen g => g -> Round -> [Question] -> [Profile] -> Map (Question,Profile) (Maybe Text)
generatePlayerQuestions :: RandomGen g => g -> Round -> [Question] -> [Profile] -> Map (Question,Profile) Answer
generatePlayerQuestions g round questions players = M.fromList $ 
  case round of
    RoundThree -> map (\p -> ((head questions, p), emptyAnswer))  players
    _ ->  fst_5 $  foldl pairPlayers ([], questions, length players - 1, Nothing, Nothing) randomPlayers

  where
    fst_5 (a,_,_,_,_) = a
    randomPlayers =  flip evalRand g $ shuffle players
        
  -- | takes the remaining questions, the previous two players of the game, and returns the newly assigned questions
  -- cons'd onto the accumulator
pairPlayers :: ([((Question,Profile) , Answer)] , [Question] , Int, Maybe Profile, Maybe Profile)
  -> Profile
  ->  ([((Question,Profile) , Answer)] , [Question] , Int, Maybe Profile, Maybe Profile) 
pairPlayers (a, qs, n, Nothing , Nothing)  p3 =
  (a, qs, n-1, Nothing, Just p3)
pairPlayers (a, qs, n, Nothing, Just p2)  p3
  | n == 0 = (a ++  assignQuestion (head qs) p2 p3 ++ assignQuestion (head $ tail qs) p2 p3, drop 2 qs, n-1, Just p2, Just p3) -- only occurs for the 2nd element of the list
  | otherwise = (a ++  assignQuestion (head qs) p2 p3, tail qs, n-1, Just p2, Just p3) 
pairPlayers (a, qs, n, Just p1 , Just p2)  p3 
  | n == 0 = (a ++ assignQuestion (head qs) p1 p3 ++ assignQuestion (head $ tail qs) p2 p3, drop 2 qs, n-1, Just p2, Just p3)
  | otherwise =   (a ++ assignQuestion (head qs) p1 p3,  qs, n-1, Just p2, Just p3)
pairPlayers other _ =  other -- this case shouldn't be possible

assignQuestion :: Question -> Profile -> Profile -> [((Question,Profile), Answer)]
assignQuestion q p1 p2 = [((q, p1),  emptyAnswer), ((q, p2),  emptyAnswer)] 


  -- | https://wiki.haskell.org/Random_shuffle 
shuffle :: RandomGen g => [a] -> Rand g [a]
shuffle xs = do
    let l = length xs
    rands <- forM [0..(l-2)] $ \i -> getRandomR (i, l-1)
    let ar = runSTArray $ do
          ar <- thawSTArray $ listArray (0, l-1) xs
          forM_ (zip [0..] rands) $ \(i, j) -> do
            vi <- readSTArray ar i
            vj <- readSTArray ar j
            writeSTArray ar j vi
            writeSTArray ar i vj
          return ar
    return (elems ar)
