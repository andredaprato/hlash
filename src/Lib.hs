{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
module Lib where


import           Network.Wai.Handler.Warp (run)
import           System.Remote.Monitoring (forkServerWith)

import qualified Data.HashMap.Strict as HashMap
import           Lib.App (AppEnv, Env(..), RoomState(..))
import           Lib.Config
import           Lib.Core.Lobby
import           Lib.Core.Types
import           Lib.Db
import           Lib.Db.Functions (initialisePool)
import           Lib.Db.Settings
import           Lib.Server (application)
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

    secretFile <- fileExist ".jwtSecret"
    envJWTSecret <- if secretFile
                    then readKey ".jwtSecret"
                    else do key <-  generateKey
                            writeKey "../.jwtSecret"
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
