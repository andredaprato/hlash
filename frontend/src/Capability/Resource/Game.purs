module Capability.Resource.Game where


import Data.Lobby
import Prelude

import Data.Profile (Profile)
import Data.Question (Question)
import Data.RoomCode (RoomCode(..))
import Halogen (HalogenM(..), lift)

class Monad m <= ManageGame m where
  submitAnswer :: Question -> String -> RoomCode -> m Unit
  voteForQuestion :: Question -> Profile -> RoomCode -> m Unit

  -- | This instance lets us avoid having to use `lift` when we use these functions in a component.
instance manageUserHalogenM :: ManageGame m => ManageGame (HalogenM st act slots msg m) where
  submitAnswer q a = lift <<< submitAnswer q a
  voteForQuestion q p = lift <<< voteForQuestion q p 
