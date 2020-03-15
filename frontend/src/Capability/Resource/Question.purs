module Capability.Resource.Question where

import Data.Question
import Prelude

import Data.Maybe (Maybe)
import Halogen (HalogenM(..), lift)

class Monad m <= ManageQuestion m where
  makeQuestion :: Question ->  m Unit
  getQuestions :: m (Maybe (Array Question))

-- | This instance lets us avoid having to use `lift` when we use these functions in a component.
instance manageUserHalogenM :: ManageQuestion m => ManageQuestion (HalogenM st act slots msg m) where
  makeQuestion = lift <<< makeQuestion
  getQuestions = lift getQuestions
