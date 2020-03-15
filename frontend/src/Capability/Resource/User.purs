module Capability.Resource.User where

import Prelude

import API.Request (LoginFields, RegisterFields)
import Data.Maybe (Maybe)
import Data.Profile (Profile)
import Halogen (HalogenM, lift)

class Monad m <= ManageUser m where
  loginUser :: LoginFields -> m (Maybe Profile)
  registerUser :: RegisterFields -> m (Maybe Profile)
  logoutUser :: m Unit

  -- | This instance lets us avoid having to use `lift` when we use these functions in a component.
instance manageUserHalogenM :: ManageUser m => ManageUser (HalogenM st act slots msg m) where
  loginUser = lift <<< loginUser
  registerUser = lift <<< registerUser
  logoutUser = lift logoutUser

