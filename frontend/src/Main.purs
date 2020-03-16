module Main where

import Data.Profile
import Env
import Prelude
import WebSocket

import API.Endpoint (Endpoint(..))
import API.Request (BaseURL(..), RequestMethod(..), WsURL(..), defaultRequest)
import API.Utils (authenticate)
import Affjax (printError, request)
import AppM (runAppM, AppM)
import Component.Router as Router
import Data.Argonaut (Json, decodeJson, encodeJson, stringify)
import Data.Argonaut.Decode (class DecodeJson)
import Data.Argonaut.Encode (class EncodeJson)
import Data.Bifunctor (bimap)
import Data.Either (Either(..), hush)
import Data.Game (Game(..), GameState(..), Round(..), Stage(..))
import Data.Maybe (Maybe(..))
import Data.RoomCode (RoomCode(..), retreiveRoomCode)
import Data.Route (router)
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Aff (Aff, forkAff, launchAff_)
import Effect.Aff.Bus as Bus
import Effect.Aff.Class (liftAff)
import Effect.Class.Console (log)
import Effect.Ref as Ref
import Effect.Var (($=), get)
import Halogen (liftEffect)
import Halogen as H
import Halogen.Aff as HA
import Halogen.HTML as HH
import Halogen.VDom.Driver (runUI)
import Routing.Duplex (parse)
import Routing.Hash (matchesWith)

main :: Effect Unit
main = HA.runHalogenAff $ do
  body <- HA.awaitBody
  
  let baseUrl = BaseURL "http://localhost:8000/api/v1"
      wsUrl   = URL "ws://localhost:8000/api/v1/stream"
      logLevel = Dev


  currentUser <- liftEffect $ Ref.new Nothing
  userBus <- liftEffect $ Bus.make
  roomCode <- liftEffect $ retreiveRoomCode

  -- check if we already have user credentials
  let requestOptions = { endpoint: IsLoggedIn, method: Get }
  res <- liftAff $ request $ defaultRequest baseUrl true requestOptions
  let u = decodeJson =<< bimap printError _.body res 
  liftEffect $ Ref.write (hush u) currentUser

  wsConn <- liftEffect $ Ref.new (Nothing)

  let environment :: Env
      environment = { baseUrl, logLevel, userEnv, wsUrl, wsConn } 
                    
        where userEnv :: UserEnv
              userEnv = { currentUser, userBus , roomCode }

      rootComponent :: H.Component HH.HTML Router.Query {} Void Aff
      rootComponent = H.hoist (runAppM environment)  Router.component
  
  driver <- runUI rootComponent {} body

  void $ liftEffect $ matchesWith (parse router) \old new ->
    when (old /= Just new) do
      launchAff_ $ driver.query $ H.tell $ Router.GoTo new

    


  pure unit


