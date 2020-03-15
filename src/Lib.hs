{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
module Lib where


import           Network.Wai.Handler.Warp (run)
import           System.Remote.Monitoring (forkServerWith)

import qualified Data.HashMap.Strict as HashMap
import           Hlash.App (AppEnv, Env(..), RoomState(..))
import           Hlash.Config
import           Hlash.Core.Lobby
import           Hlash.Core.Types
import           Hlash.Db
import           Hlash.Db.Functions (initialisePool)
import           Hlash.Db.Settings
import           Hlash.Server (application)
import           Servant.Auth.Server (generateKey, writeKey, readKey, defaultJWTSettings, defaultCookieSettings, CookieSettings(..), SameSite(..), IsSecure(..))
import qualified System.Metrics as Metrics
import           System.Posix.Files (fileExist)

mkAppEnv :: Config -> IO AppEnv
mkAppEnv Config{..} = do
    -- IO configuration
    envDbPool   <- initialisePool cDbCredentials
    envSessions <- newMVar HashMap.empty
    envTimings  <- newIORef HashMap.empty
    envEkgStore <- Metrics.newStore
    gameStore <- newTVarIO HashMap.empty
    lobbyStore <- newTVarIO HashMap.empty
    clients <- newTVarIO HashMap.empty
    let envRoomState =  RoomState {lobbyStore , gameStore,  clients}

    secretFile <- fileExist "./.jwtSecret"
    envJWTSecret <- if secretFile
                    then readKey "./.jwtSecret"
                    else do key <-  generateKey
                            writeKey "./.jwtSecret"
                            pure key


    -- pure configuration
    let envSessionExpiry = 600
        envJWTSettings = defaultJWTSettings envJWTSecret
        envCookieSettings = defaultCookieSettings { cookieSameSite = AnySite,
                                                    cookieIsSecure = NotSecure,
                                                    cookieXsrfSetting = Nothing,
                                                    cookiePath = Just "/"}
    env <- pure Env {..} 
    runReaderT seedDefaultQuestions env 
    pure $ env

runServer :: AppEnv -> IO ()
runServer env@Env{..} = do
    -- configure and run EKG
    Metrics.registerGcMetrics envEkgStore
    () <$ forkServerWith envEkgStore "localhost" 8081
  
    run 8000 $ application env 
    

  
main :: IO ()
main = loadConfig >>= mkAppEnv >>= runServer
