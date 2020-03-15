{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
module Lib.Db.Users.Question where

import Database.Beam
import Lib.Db.Users.User  
import Data.Aeson

data QuestionT f = Question
  {
    _questionId   :: C f Int
  , _questionBody :: C f Text
  , _questionUser :: PrimaryKey UserT f 
  } deriving (Generic, Beamable)

type QuestionId = PrimaryKey QuestionT Identity
type Question =  QuestionT Identity

Question (LensFor questionId) (LensFor questionBody) (UserId (LensFor questionUser)) =
     tableLenses

deriving instance Show Question 
deriving instance Eq Question 
deriving instance Ord Question 
deriving instance FromJSON Question 
deriving instance ToJSON Question
deriving instance ToJSONKey Question
deriving instance FromJSONKey Question 


deriving instance Eq (PrimaryKey QuestionT Identity)
deriving instance Show (PrimaryKey QuestionT Identity)
deriving instance Ord (PrimaryKey QuestionT Identity)
deriving instance ToJSON (PrimaryKey QuestionT Identity)
deriving instance FromJSON (PrimaryKey QuestionT Identity)
deriving instance ToJSONKey (PrimaryKey QuestionT Identity)
deriving instance FromJSONKey (PrimaryKey QuestionT Identity)

instance Table QuestionT  where
  data PrimaryKey QuestionT f = QuestionId (C f Int) deriving (Generic, Beamable)
  primaryKey = QuestionId . _questionId

  
