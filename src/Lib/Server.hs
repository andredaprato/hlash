{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE OverloadedStrings #-}

module Lib.Server where

-- import Lib.Server.Auth (AuthApi, authServer)
-- import Lib.S (ToApi)
import Data.ByteString
import Lib.Server.Site (Site(..), SiteApi, siteServer)
import Lib.App (runAppAsHandler, AppEnv)
import Lib.App.Env(Env(..))
import Network.Wai as Wai
import Network.Wai.Middleware.Cors

import Servant.Server (Server, Application, hoistServer, serve)
import Servant.Auth.Server 
import Servant
import Crypto.JOSE.JWK (JWK)
import Servant.API.WebSocket
import Network.WebSockets.Connection
import Control.Concurrent
import Data.Text as T


type Api = SiteApi 

server :: AppEnv -> Server Api 
server env =
  hoistServerWithContext
    (Proxy @Api )
    (Proxy @'[JWTSettings, CookieSettings])
    (runAppAsHandler env)
    (toServant $ siteServer (envCookieSettings env) (envJWTSettings env))
    -- $ siteServer (envCookieSettings env) (envJWTSettings env)

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
    -- corsOrigins        = Just (["http://localhost:1234"], True)
    corsOrigins        = Just (["http://localhost:1234", "http://localhost:35321"], True)
  , corsMethods        = ["OPTIONS", "GET", "PUT", "POST", "PATCH", "DELETE"]
  -- , corsRequestHeaders = ["Authorization", "Content-Type", "Cookie"]
  , corsRequestHeaders = ["Content-Type", "Cookie"]
  , corsExposedHeaders = Nothing
  , corsMaxAge         = Nothing
  , corsVaryOrigin     = False
  , corsRequireOrigin  = False
  , corsIgnoreFailures = False
}
