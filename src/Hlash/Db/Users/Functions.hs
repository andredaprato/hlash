{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
module Hlash.Db.Users.Functions where 


import Database.Beam
import Database.Beam.Postgres
import Database.Beam.Backend.SQL.BeamExtensions


import Hlash.Db.Settings
import Hlash.Db.Users.User
import Hlash.Db.Users.Question
import Hlash.Db.Functions


lookupUsername  :: WithDb env m => Text -> m (Maybe User)
lookupUsername username = runBeamWithPool $ runSelectReturningOne $
    lookup_ (lashDb ^. lashUsers) (UserId username)
  
insertUser :: WithDb env m => User -> m ()
insertUser user@User{..} = runBeamWithPool $ runInsert $

  insert (lashDb ^. lashUsers) $ insertValues [user]

insertQuestion :: WithDb env m => UserId -> Text -> m ()
insertQuestion user question = runBeamWithPool $ runInsert $
  insert (_lashQuestions lashDb) $
  insertExpressions [Question { _questionId = default_
                              , _questionBody = val_ question
                              , _questionUser = val_ user
                              }
                    ]


getUserQuestions :: WithDb env m => UserId -> m [Question]
getUserQuestions user = runBeamWithPool $ runSelectReturningList $
  select $ do someUser <- all_ (lashDb ^. lashUsers) 
              someQuestion <- all_ (lashDb ^. lashQuestions) 
              guard_ (pk someUser ==. val_ user)
              guard_ (_questionUser someQuestion `references_` someUser)
              return someQuestion
