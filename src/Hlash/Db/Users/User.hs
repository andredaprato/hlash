{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ImpredicativeTypes #-}
module Hlash.Db.Users.User (UserT(..), User, UserId, PrimaryKey(UserId), mockUser) where

import Database.Beam
import Servant.Auth.Server

import Hlash.Core.Password (PasswordHash)

data UserT f = User { _userName :: C f Text
                    , _userPass   :: C f PasswordHash
             } deriving (Generic, Beamable)


User (LensFor userName) (LensFor userPass) =
     tableLenses

instance Table UserT  where
  data PrimaryKey UserT f = UserId (C f Text) deriving (Generic, Beamable)
  primaryKey = UserId . _userName


type User = UserT Identity
type UserId = PrimaryKey UserT Identity

deriving instance Show User
deriving instance Eq User
deriving instance Ord User
deriving instance ToJSON User
deriving instance FromJSON User

deriving instance ToJWT UserId
deriving instance FromJWT UserId

deriving instance Eq (PrimaryKey UserT Identity)
deriving instance Show (PrimaryKey UserT Identity)
deriving instance Ord (PrimaryKey UserT Identity)
deriving instance ToJSON (PrimaryKey UserT Identity)
deriving instance FromJSON (PrimaryKey UserT Identity)

mockUser :: User
mockUser = User "andre" "a"
