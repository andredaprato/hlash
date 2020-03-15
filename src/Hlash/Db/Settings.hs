{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ImpredicativeTypes #-}
module Hlash.Db.Settings where

import Database.Beam
import Database.Beam.Postgres
import qualified Data.Pool as Pool

import Hlash.Db.Functions (withPool, WithDb)
import Hlash.App.Env (DbPool)
import Hlash.Db.Users.User
import Hlash.Db.Users.Question
import Hlash.Db.DefaultQuestions

import Hlash.Core.Password

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

