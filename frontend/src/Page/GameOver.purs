module Page.GameOver where

import Prelude

import Data.Array (head, sortBy, tail)
import Data.Game (Score)
import Data.Map (Map)
import Data.Map as M
import Data.Maybe (Maybe(..))
import Data.Ord (compare)
import Data.Profile (Profile)
import Data.Tuple (snd, uncurry)
import Data.Username (toString)
import Halogen as H
import Halogen.HTML as HH
import UI.UI (column, css)



renderGameOver :: forall m act slots . Map Profile Score ->  H.ComponentHTML act slots m
renderGameOver scores =

  HH.div_  [
    HH.p  [css "title"]
    [
      HH.text "Game Over!"
    ]
    , HH.p [ css "subtitle"]
    [
      HH.text "Final Scores"
    ]
    , HH.hr [ css "has-background-info"]

    , HH.div [ css "columns is-multiline" ] $
    map (\score -> uncurry renderScore score) sortedScores

    ]

  
  where
    renderScore profile score =
      column "is-half" 
      [
        HH.div [ css "notification is-primary" ]
        [
          HH.h1 [ css "title" ]
          [
            HH.text $ toString profile.username 
          ]
        , HH.p [ css "has-text-centered is-size-1"]
          [
              HH.text $ show score 
            ]
        ]
      ]

    sortedScores = sortBy (\a b -> compare (snd a) (snd b) ) $ M.toUnfoldable scores
