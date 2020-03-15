{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ImpredicativeTypes #-}
module Lib.Db.Settings where

import Database.Beam
import Database.Beam.Postgres
import qualified Data.Pool as Pool

import Lib.Db.Functions (withPool, WithDb)
import Lib.App.Env (DbPool)
import Lib.Db.Users.User
import Lib.Db.Users.Question
import Lib.Db.DefaultQuestions

import Lib.Core.Password

data LashDb f =
  LashDb { _lashUsers ::  f (TableEntity UserT)
         , _lashQuestions :: f (TableEntity QuestionT)
         , _lashDefaultQuestions :: f (TableEntity DefaultQuestionT)
                               } deriving (Generic, Database be)

lashDb :: DatabaseSettings be LashDb
lashDb = defaultDbSettings


-- generates lenses
LashDb (TableLens lashUsers)
  (TableLens lashQuestions)
  (TableLens lashDefaultQuestions)
  = dbLenses

