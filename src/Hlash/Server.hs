{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE OverloadedStrings #-}

module Hlash.Server where

import Data.ByteString
import Hlash.Server.Site (Site(..), SiteApi, siteServer)
import Hlash.App (runAppAsHandler, AppEnv)
import Hlash.App.Env (Env(..))
import Network.Wai as Wai
import Network.Wai.Middleware.Cors

import Control.Concurrent
import Crypto.JOSE.JWK (JWK)
import Data.Text as T
import Network.WebSockets.Connection
import Servant
import Servant.API.WebSocket
import Servant.Auth.Server
import Servant.Server (Server, Application, hoistServer, serve)


type Api = SiteApi 

server :: AppEnv -> Server Api 
server env =
  hoistServerWithContext
    (Proxy @Api )
    (Proxy @'[JWTSettings, CookieSettings])
    (runAppAsHandler env)
    (toServant $ siteServer (envCookieSettings env) (envJWTSettings env))

application ::  AppEnv -> Application
application env =
  let jwtCfg = envJWTSettings env
      cfg = envCookieSettings env :. jwtCfg :. EmptyContext
  in
  corsified $ serveWithContext (Proxy @Api ) cfg (server env)


corsified :: Wai.Middleware
corsified = cors (const $ Just appCorsResourcePolicy)

combineMiddleware :: Wai.Middleware -> IO Wai.Middleware -> IO Wai.Middleware
combineMiddleware a = fmap (. a)

appCorsResourcePolicy :: CorsResourcePolicy
appCorsResourcePolicy = CorsResourcePolicy {
    corsOrigins        = Just (["http://localhost:1234"], True)
  , corsMethods        = ["OPTIONS", "GET", "PUT", "POST", "PATCH", "DELETE"]
  , corsRequestHeaders = ["Content-Type", "Cookie"]
  , corsExposedHeaders = Nothing
  , corsMaxAge         = Nothing
  , corsVaryOrigin     = False
  , corsRequireOrigin  = False
  , corsIgnoreFailures = False
}
