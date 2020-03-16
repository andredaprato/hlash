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
module Hlash.Server.Site where

import Hlash.Server.Authentication ( AuthSite(..)
                                 , ProtectedAuthSite(..)
                                 , unprotectedAuthServer
                                 , protectedAuthServer)
import Hlash.Server.Question (QuestionRoute(..) , questionServer)
import Hlash.Server.Socket (SocketSite(..), socketServer)
import Hlash.Server.Types (AppServer, withAuthResult)
import Hlash.App.Monad (App)
import Hlash.Core.Profile
import Hlash.Db (UserT(..), UserId)

import Servant
import Servant.API.Generic (ToServantApi, ToServant,  toServant, genericApi, fromServant)
import Servant.Auth.Server (Auth, AuthResult(..), Cookie, JWT, CookieSettings, JWTSettings, throwAll)
import Servant.Server.Generic (genericServer)

type SiteApi = ToServantApi Site

type SiteAuths = Auth '[Cookie]
data Site route = Site
  {
    protectedSite :: route :- "api" :> "v1" :> SiteAuths UserId :> ToServantApi ProtectedSite 
  , unprotectedSite :: route :- "api" :> "v1" :> ToServantApi UnprotectedSite 
  , assets :: route :- Raw
  } deriving (Generic)


data UnprotectedSite route = UnprotectedSite
  {
    authSite :: route :- ToServantApi AuthSite 
  } deriving (Generic)

data ProtectedSite route = ProtectedSite
  {
    protectedAuthSite :: route :- ToServantApi ProtectedAuthSite 
  , questionRoute :: route :- ToServantApi QuestionRoute 
  , socketSite :: route :- ToServantApi SocketSite 
  } deriving (Generic)

-- TODO: figure out how to handle the auth result better than this
protectedServer ::  AuthResult UserId -> ProtectedSite AppServer
protectedServer  res =  ProtectedSite
  { protectedAuthSite =  toServant $ protectedAuthServer res
  , questionRoute = toServant $ questionServer  res
  , socketSite = toServant $ socketServer  res
  }

unprotectedServer :: CookieSettings -> JWTSettings -> UnprotectedSite AppServer
unprotectedServer cs jwts =  UnprotectedSite
                             { authSite = toServant $ unprotectedAuthServer cs jwts
                             }
  
siteServer :: CookieSettings -> JWTSettings -> Site AppServer 
siteServer cs jwts =
  Site { protectedSite =  toServant . protectedServer
       , unprotectedSite = toServant $ unprotectedServer cs jwts
       , assets = serveDirectoryFileServer "assets"
       }

