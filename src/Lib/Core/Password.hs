{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Lib.Core.Password
       (
         -- PasswordHash (unPasswordHash)
         PasswordHash 
       , PasswordPlainText (..)
       -- , unsafePwdHash
       , mkPasswordHashWithPolicy
       , mkPasswordHash
       , verifyPassword
       ) where

import Lib.App.Error (AppErrorType, WithError, serverError, throwOnNothingM)

import qualified Crypto.BCrypt as BC
import Database.Beam.Postgres
import Database.Beam.Backend.SQL
import Database.Beam


-- | Password hash.
-- newtype PasswordHash = PasswordHash
--     { unPasswordHash :: Text
--     } deriving stock (Generic)
--       deriving newtype (Show, Eq, FromField, ToField, FromJSON, ToJSON, FromBackendRow Postgres)
type PasswordHash = Text

-- | Unsafe function for constructing 'PasswordHash'. Used mostly for testing.
-- unsafePwdHash :: Text -> PasswordHash
-- unsafePwdHash = PasswordHash

-- | Password in plain text.
newtype PasswordPlainText = PasswordPlainText
    { unPasswordPlainText :: Text
    } deriving stock (Show, Generic)
      deriving newtype (Eq, FromJSON, ToJSON)


-- | Generates a password hash given the hashing policy and its plane text.
-- This has to be done in IO asy generating the salt requires RNG.
mkPasswordHashWithPolicy
    :: forall m . (WithError m, MonadIO m)
    => BC.HashingPolicy
    -> PasswordPlainText
    -> m PasswordHash
mkPasswordHashWithPolicy hashPolicy password = throwOnNothingM errorMessage hashText
  where
    hashBS :: m (Maybe ByteString)
    hashBS = liftIO $ BC.hashPasswordUsingPolicy
        hashPolicy
        (encodeUtf8 $ unPasswordPlainText password)

    hashText :: m (Maybe PasswordHash)
    hashText =  decodeUtf8 <<$>> hashBS

    errorMessage :: AppErrorType
    errorMessage = serverError "Error generating password hash"

-- | Generates the password hash with slow hashing policy.
mkPasswordHash :: (WithError m, MonadIO m) => PasswordPlainText -> m PasswordHash
mkPasswordHash = mkPasswordHashWithPolicy BC.slowerBcryptHashingPolicy

verifyPassword :: PasswordPlainText -> PasswordHash -> Bool
verifyPassword (PasswordPlainText password)  hash =
    BC.validatePassword (encodeUtf8 hash) (encodeUtf8 password)
