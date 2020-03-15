{-# LANGUAGE TypeOperators #-}

{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE StandaloneDeriving #-} 

module Hlash.Server.Question where


import           Hlash.App.Env (grab, Has, Env)
import           Hlash.App.Error (WithError, invalid, notAllowed, throwError, throwOnNothingM)
import           Hlash.Core.Password (PasswordHash(..), PasswordPlainText(..), mkPasswordHash, verifyPassword)
import           Hlash.Core.Question
import           Hlash.Db (WithDb, lookupUsername, insertUser, UserT(..), User, UserId)
import qualified Hlash.Db as Db
import           Hlash.Effects.Measure (MonadMeasure(..), timedAction)
import           Hlash.Server.Types (AppServer, CookieHeaders, withAuthResult)
import           Hlash.Time (dayInSeconds)

import           Servant.API.Generic (ToServant, AsApi)
import           Servant.Auth.Server
import           Servant (PostNoContent, Headers)

type Amount = Text

data QuestionRoute route = QuestionRoute {
  insertQuestion  :: route
    :- "question"
    :> ReqBody '[JSON] Question
    :> Post '[JSON] NoContent
  , getQuestions :: route
    :- "question"
    :> QueryParam "type" Amount
    :> Get '[JSON] [Question]
                     } deriving (Generic)


questionServer ::  AuthResult UserId -> QuestionRoute AppServer
questionServer  f =
  QuestionRoute  { insertQuestion = insertQuestionHandler f 
                 , getQuestions = getQuestionHandler f
                  }

insertQuestionHandler
  :: ( WithDb env m
     , MonadMeasure m
     , WithError m
     )
  => AuthResult UserId -> Question -> m NoContent
insertQuestionHandler (Authenticated user) (Question questionBody) = timedAction $ do
  Db.insertQuestion user $ questionBody 
  pure NoContent
insertQuestionHandler _ _  = throwError $ notAllowed "unauthorized"


getQuestionHandler
  :: ( WithDb env m
     , MonadMeasure m
     , WithError m)
  => AuthResult UserId -> Maybe Amount ->  m [Question]
  -- TODO: handle requests for different types of lists of questions
getQuestionHandler (Authenticated user) _  = timedAction $ do
  Db.getUserQuestions  user >>= mapM (\q -> pure $ Question $ Db._questionBody q)
getQuestionHandler _ _  = throwError $ notAllowed "unauthorized"

  
  
     

