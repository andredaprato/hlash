-- | This module exports various utilities for working with a REST API and Json. It also provides
-- | a few helpers shared among requests which I found useful when implementing the production
-- | monad, `Conduit.AppM`.
module API.Utils where

import Prelude

import API.Request (BaseURL, RequestOptions, defaultRequest)
import Affjax (request)
import Control.Monad.Reader (class MonadAsk, ask, asks)
import Data.Argonaut (class DecodeJson, decodeJson)
import Data.Argonaut.Core (Json)
import Data.Bifunctor (rmap)
import Data.Either (Either(..), hush)
import Data.Maybe (Maybe(..))
import Data.Profile (Profile)
import Data.Tuple (Tuple(..))
import Data.Username (Username)
import Effect.Aff.Bus as Bus
import Effect.Aff.Class (class MonadAff, liftAff)
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Class.Console (log)
import Effect.Ref as Ref
import Env (UserEnv)

-- | This function performs a request that does not require authentication by pulling the base URL
-- | out of the app environment and running an asynchronous request. This function only requires the
-- | `baseUrl` field from the app environment. See `Conduit.AppM` for examples of this in action.
mkRequest
  :: forall m r
   . MonadAff m
  => MonadAsk { baseUrl :: BaseURL | r } m
  => RequestOptions
  -> m (Maybe Json)
mkRequest opts = do
  { baseUrl } <- ask
  response <- liftAff $ request $ defaultRequest baseUrl false opts
  pure $ hush $ rmap _.body response

-- | This function performs a request that requires authentication by pulling the base URL out
-- | of the app environment, reading the auth token from local storage, and then performing
-- | the asynchronous request. See `Conduit.AppM` for examples of this in action.
mkAuthRequest
  :: forall m r
   . MonadAff m
  => MonadAsk { baseUrl :: BaseURL | r } m
  => RequestOptions
  -> m (Maybe Json)
mkAuthRequest opts = do
  { baseUrl } <- ask
  response <- liftAff $ request $ defaultRequest baseUrl true opts
  pure $ hush $ rmap _.body response

authenticate 
  :: forall m a r
     . MonadAff m
     => MonadAsk { baseUrl :: BaseURL, userEnv :: UserEnv | r } m
  => (BaseURL -> a -> m (Either String Profile))
  -> a 
  -> m (Maybe Profile)
authenticate req fields = do 
  { baseUrl, userEnv } <- ask
  req baseUrl fields >>= case _ of
    Left err -> liftAff (log err) *> pure Nothing
    Right profile -> do 
      liftEffect $
        Ref.write (Just profile) userEnv.currentUser
      -- any time we write to the current user ref, we should also broadcast the change 
      liftAff $  Bus.write (Just profile) userEnv.userBus
      pure (Just profile)


decode :: forall m a . MonadAff m => DecodeJson a => Maybe Json -> m (Maybe a)
decode Nothing = log "response is not valid json" *> pure Nothing
decode (Just val) = case decodeJson val of
  Left err -> log err *> pure Nothing
  Right succ -> pure $ Just succ
  


