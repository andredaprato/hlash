-- https://web.archive.org/web/20190819000310/https://www.trueachievements.com/a208499/quiplash-xl-back-talk-achievement
-- a list of questions to seed our database. I'm not sure what the copyright
-- implications of doing this are...
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}
module Hlash.Db.Seed where
import Prelude hiding (many, optional, (<|>) )

import qualified Data.Text as T
import Text.Parsec
import Text.Parsec.Char
import Text.HTML.Scalpel as Scalpel

import Hlash.Db.Functions as F
import Hlash.Db.Settings
import Hlash.Db.DefaultQuestions
import Hlash.Core.Question as Core

import System.Random
import Database.Beam
import Database.Beam.Postgres
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.ToField
import Database.PostgreSQL.Simple.FromField

import Database.PostgreSQL.Simple.SqlQQ (sql)

seedDefaultQuestions :: (WithDb env m, MonadIO m) => m ()
seedDefaultQuestions = do
  mQs <- liftIO $ parseQuestions
  case mQs of
    Nothing -> liftIO $ print "can't fetch default questions"
    Just qs -> insertDefaultQuestions qs
  
    
questions = "https://web.archive.org/web/20190819000310/https://www.trueachievements.com/a208499/quiplash-xl-back-talk-achievement"

getQuestions :: IO (Maybe [Text])
getQuestions = scrapeURL questions comments
  where comments = chroots ("td" @: [hasClass "message"]) (Scalpel.text anySelector)

parseQuestions :: IO (Maybe [Text])
parseQuestions = do
  mQuestions <- getQuestions 
  case mQuestions of
    Nothing -> pure Nothing
    Just questions ->
      let b = parse parseQuestions "" <$> questions
      in pure
         $ fmap concat
         $ forM b
         $ \elem -> case elem of 
                      Left err -> Nothing
                      Right thing -> Just $ fmap (T.replace "\"BLANK\"" "_____") $ T.pack <$> thing

  where parseQuestions =  many $ do
          try (manyTill anyChar (try $ string "Question:")) <|> (eof >> pure [])
          try (manyTill anyChar ((try $ string "Answer"))) <|> (eof >> pure [])


-- | Inserts the input questions if the default questions table is not already
-- | populated
insertDefaultQuestions :: WithDb env m => [Text] -> m ()
insertDefaultQuestions qs = runBeamWithPool $ 
  runInsert $ insert (_lashDefaultQuestions lashDb)
  $ insertFrom $ do vals <- values_ expr
                    guard_ (not_ $ exists_  $ all_ (_lashDefaultQuestions lashDb))
                    pure vals
                    

  where  expr = flip map (zip [1..] qs)  $ \(i,  q ) ->  DefaultQuestion (val_ i) (val_ q) 

-- |  beam doesn't seem to have the postgres random function unfortunately,
-- so we resort to postgresql-simple for the time being
getDefaultQuestions :: (WithDb env m) => Int -> m [Text]
getDefaultQuestions num = fromOnly <<$>>  
  F.query  [sql|
             select question_body
             from default_questions
             order by random()
             limit ?
             |]  (Only num) 
