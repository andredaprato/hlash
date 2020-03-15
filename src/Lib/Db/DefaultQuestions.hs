{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
module Lib.Db.DefaultQuestions where



import Database.Beam
import Lib.Db.Users.User  
import Data.Aeson

data DefaultQuestionT f = DefaultQuestion
  {
    _defaultQuestionId   :: C f Int
  , _defaultQuestionBody :: C f Text
  } deriving (Generic, Beamable)

type DefaultQuestionId = PrimaryKey DefaultQuestionT Identity
type DefaultQuestion =  DefaultQuestionT Identity

DefaultQuestion (LensFor defaultQuestionId) (LensFor defaultQuestionBody) =
     tableLenses

deriving instance Show DefaultQuestion 
deriving instance Eq DefaultQuestion 
deriving instance Ord DefaultQuestion 
deriving instance FromJSON DefaultQuestion 
deriving instance ToJSON DefaultQuestion
deriving instance ToJSONKey DefaultQuestion
deriving instance FromJSONKey DefaultQuestion 


deriving instance Eq (PrimaryKey DefaultQuestionT Identity)
deriving instance Show (PrimaryKey DefaultQuestionT Identity)
deriving instance Ord (PrimaryKey DefaultQuestionT Identity)
deriving instance ToJSON (PrimaryKey DefaultQuestionT Identity)
deriving instance FromJSON (PrimaryKey DefaultQuestionT Identity)
deriving instance ToJSONKey (PrimaryKey DefaultQuestionT Identity)
deriving instance FromJSONKey (PrimaryKey DefaultQuestionT Identity)

instance Table DefaultQuestionT  where
  data PrimaryKey DefaultQuestionT f = DefaultQuestionId (C f Int) deriving (Generic, Beamable)
  primaryKey = DefaultQuestionId . _defaultQuestionId
