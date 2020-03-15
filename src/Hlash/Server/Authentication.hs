{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
module Hlash.Server.Authentication where

import Control.Concurrent
import Data.Text as T
import Database.Beam
import Hlash.App.Env (grab, Has, Env)
import Hlash.App.Error (WithError, invalid, notAllowed, throwError, throwOnNothingM)
import Hlash.Core.Password (PasswordHash(..), PasswordPlainText(..), mkPasswordHash, verifyPassword)
import Hlash.Core.Profile
import Hlash.Db (WithDb, lookupUsername, insertUser, UserT(..), User)
import Hlash.Db as Db
import Hlash.Effects.Measure (MonadMeasure(..), timedAction)
import Hlash.Server.Types
import Hlash.Server.Types (AppServer, CookieHeaders)
import Hlash.Time (dayInSeconds)
import Network.WebSockets.Connection
import Servant (PostNoContent, Headers)
import Servant.API.Generic (ToServant, AsApi)
import Servant.API.WebSocket
import Servant.Auth.Server


data LoginRequest = LoginRequest
     { username    :: Text -- type alias this?
     , password :: PasswordPlainText
    } deriving (Generic, Show, Eq)

instance FromJSON LoginRequest where
instance ToJSON LoginRequest where
instance FromJWT LoginRequest where
instance ToJWT LoginRequest where

data AuthSite route = AuthSite {
  loginRoute :: route
    :- "login"
    :> ReqBody '[JSON] LoginRequest
    :> Post '[JSON] (CookieHeaders Profile),
  registerRoute :: route
    :- "register"
    :> ReqBody '[JSON] LoginRequest
    :> Post '[JSON] (CookieHeaders Profile)
  } deriving (Generic)


data ProtectedAuthSite route = ProtectedAuthSite {
  logoutRoute :: route
    :- "logout"
    :> Get '[JSON] (CookieHeaders NoContent),
  isLoggedInRoute :: route
    :- "isLoggedIn"
    :> Get '[JSON] Profile
  } deriving (Generic)

unprotectedAuthServer :: CookieSettings -> JWTSettings -> AuthSite AppServer
unprotectedAuthServer cs jwts = AuthSite
            { loginRoute = loginHandler cs jwts
            , registerRoute = registrationHandler cs jwts
            }

protectedAuthServer :: AuthResult UserId ->  ProtectedAuthSite AppServer
protectedAuthServer f = ProtectedAuthSite {
  logoutRoute = withAuthResult logoutHandler f,
  isLoggedInRoute = withAuthResult isLoggedInHandler f
                      }

loginHandler
    :: ( MonadMeasure m
       , WithDb env m
       , WithError m
       )
    => CookieSettings
    -> JWTSettings
    -> LoginRequest ->
    m (CookieHeaders Profile)
    
loginHandler cs jwts loginReq@LoginRequest{..} = timedAction $ do
    liftIO $ putStrLn "loginHandler"
    maybeUser <- lookupUsername username
    case maybeUser of
      Nothing -> do liftIO $ putStrLn "hm"
                    throwError (notAllowed "User not found.")
      Just user@User{..} -> do
        let isPasswordCorrect = verifyPassword password _userPass
        unless isPasswordCorrect $ do
          Hlash.App.Error.throwError (notAllowed "Invalid Password")
        returnCookies cs jwts (pk user) (Profile $ username)

registrationHandler
  :: ( MonadMeasure m
     , WithDb env m
     , WithError m
     )
  => CookieSettings
  -> JWTSettings
  -> LoginRequest
  -> m (CookieHeaders Profile)
registrationHandler cs jwts registerReq@LoginRequest{..} =  timedAction $ do
  checkExists <- lookupUsername username
  case checkExists of
    Nothing -> do
      hash <- mkPasswordHash password
      let user = Db.User username  hash
      insertUser user 
      returnCookies cs jwts (pk user) (Profile $ username )

    Just _  -> throwError $ invalid  "Username is already taken"

returnCookies
  :: (
    MonadIO m,
    WithError m)
  => CookieSettings
  -> JWTSettings
  -> UserId
  -> Profile
  -> m (CookieHeaders Profile)
returnCookies cs jwts registerReq user = do 
  mApplyCookies <- liftIO $ acceptLogin cs jwts registerReq
  case mApplyCookies of
    Nothing -> throwError $ notAllowed "Failed to create cookies"
    Just applyCookies -> return $ applyCookies  user

------------------ require authentication
logoutHandler
    :: ( MonadMeasure m
       , MonadIO m
       , MonadReader env m
       , Has CookieSettings env
       )
    => 
    UserId -> m (CookieHeaders NoContent)
logoutHandler _ = timedAction $ do
  cs <- grab @CookieSettings
  pure $ clearSession cs NoContent

isLoggedInHandler :: Applicative m => UserId -> m Profile
isLoggedInHandler (UserId userid) = pure  (Profile userid)

