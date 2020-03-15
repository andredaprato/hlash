{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE StandaloneDeriving #-}
module Hlash.Effects.RoomCode ( generateCode,
                              testingCode
                            , RoomCode -- no constructor
  ) where

import System.Random
import Control.Monad
import qualified Data.Text as T
import Data.Aeson (ToJSONKey, FromJSONKey)
import Database.Beam.Postgres
import Database.Beam.Backend.SQL
import Database.Beam
-- class MonadIO m => MonadCode m where
--   getNextCode
  
newtype RoomCode = RoomCode Text deriving (Generic, Show,  Eq, Ord, FromBackendRow Postgres,
                                          FromJSON, ToJSON, ToJSONKey, FromJSONKey, Hashable)

instance HasSqlValueSyntax be String => HasSqlValueSyntax be RoomCode where
  sqlValueSyntax = autoSqlValueSyntax
deriving instance HasSqlEqualityCheck Postgres RoomCode

  -- | 65, 90 is the range of latin uppercase characters
generateCode :: MonadIO m => m RoomCode
generateCode =  liftIO $ RoomCode .  T.pack <$> replicateM 4 (toEnum <$> (randomRIO (65,90) :: IO Int)) 

testingCode = RoomCode "ABCD"
