{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DeriveGeneric #-}

module Lib.Core.Profile
       ( Profile (..)
       ) where

import Lib.Core.Password (PasswordHash)
import Servant.Auth.Server(ToJWT, FromJWT)
import Data.Aeson (ToJSONKey, FromJSONKey)


data Profile = Profile
    {
     username  :: !Text
    } deriving stock (Generic, Show, Eq, Ord)
      deriving anyclass (Hashable, ToJWT, FromJWT, FromJSON, ToJSON, ToJSONKey, FromJSONKey) 
