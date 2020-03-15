{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
module Hlash.Core.Question where

import Data.Aeson

newtype Question = Question Text
  deriving stock (Generic)
  deriving anyclass (ToJSONKey, FromJSONKey)
  deriving newtype (Eq, Show, Ord, Hashable, FromJSON, ToJSON)
