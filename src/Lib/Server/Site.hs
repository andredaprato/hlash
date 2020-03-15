{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
module Lib.Server.Site where

import Lib.Server.Authentication ( AuthSite(..)
                                 , ProtectedAuthSite(..)
                                 , unprotectedAuthServer
                                 , protectedAuthServer)
import Lib.Server.Question (QuestionRoute(..) , questionServer)
import Lib.Server.Lobby (LobbySite(..), lobbyServer)
import Lib.Server.Types (AppServer, withAuthResult)
import Lib.App.Monad (App)
import Lib.Core.Profile

import Lib.Db(UserT(..), UserId)
import Servant
import Servant.API.Generic (ToServantApi, ToServant,  toServant, genericApi, fromServant)
import Servant.Server.Generic (genericServer)
import Servant.Auth.Server(Auth, AuthResult(..), Cookie, JWT, CookieSettings, JWTSettings, throwAll)

type SiteAuths = Auth '[Cookie]
data Site route = Site
  {
    protectedSite :: route :- SiteAuths UserId :> ToServantApi ProtectedSite 
  , unprotectedSite :: route :- ToServantApi UnprotectedSite 
  } deriving (Generic)

type SiteApi = ToServantApi Site 

data UnprotectedSite route = UnprotectedSite
  {
    authSite :: route :- ToServantApi AuthSite 
  -- , client :: route :- Raw
  } deriving (Generic)

data ProtectedSite route = ProtectedSite
  {
    protectedAuthSite :: route :- ToServantApi ProtectedAuthSite 
  , questionRoute :: route :- ToServantApi QuestionRoute 
  , lobbySite :: route :- ToServantApi LobbySite 
  } deriving (Generic)

-- TODO: figure out how to handle the auth result better than this

protectedServer ::  AuthResult UserId -> ProtectedSite AppServer
protectedServer  res =  ProtectedSite
  {
    protectedAuthSite =  toServant $ protectedAuthServer res
  , questionRoute = toServant $ questionServer  res
  , lobbySite = toServant $ lobbyServer  res
  }

unprotectedServer :: CookieSettings -> JWTSettings -> UnprotectedSite AppServer
unprotectedServer cs jwts =  UnprotectedSite
                              {
                                authSite = toServant $ unprotectedAuthServer cs jwts
                             -- , client = serveDirectoryFileServer "assets"
                             }
  
siteServer :: CookieSettings -> JWTSettings -> Site AppServer 
siteServer cs jwts =
  Site { protectedSite =  toServant . protectedServer
       , unprotectedSite = toServant $ unprotectedServer cs jwts
       }
