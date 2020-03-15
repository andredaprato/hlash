{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DeriveGeneric #-}

module Hlash.Core.Profile
       ( Profile (..)
       ) where

import Hlash.Core.Password (PasswordHash)
import Servant.Auth.Server(ToJWT, FromJWT)
import Data.Aeson (ToJSONKey, FromJSONKey)


data Profile = Profile
    {
     username  :: !Text
    } deriving stock (Generic, Show, Eq, Ord)
      deriving anyclass (Hashable, ToJWT, FromJWT, FromJSON, ToJSON, ToJSONKey, FromJSONKey) 
