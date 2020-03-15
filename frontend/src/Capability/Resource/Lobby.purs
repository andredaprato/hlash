module Capability.Resource.Lobby where

import Data.Lobby
import Prelude

import Data.RoomCode (RoomCode(..))
import Halogen (HalogenM(..), lift)

class Monad m <= ManageLobby m where
  makeLobby ::  m Unit
  joinLobby :: RoomCode -> m Unit
  initGame :: Lobby -> m Unit

  -- | This instance lets us avoid having to use `lift` when we use these functions in a component.
instance manageUserHalogenM :: ManageLobby m => ManageLobby (HalogenM st act slots msg m) where
  makeLobby = lift makeLobby
  joinLobby = lift <<< joinLobby 
  initGame = lift <<< initGame
