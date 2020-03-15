-- | For now, our production app manages resources and fetches data using a REST API. This module

-- | defines several data types and helper functions to create and manage these requests. That
-- | includes managing auth tokens, designing types to represent possible requests, and more.
-- |
-- | While interesting, this module is largely mechanical. It helps provide most of the low-level
-- | functions that our production monad will leverage in `Conduit.AppM` to implement our various
-- | app capabilities.
module API.Request
  (
   BaseURL(..)
  , WsURL(..)
  , RequestMethod(..)
  , RequestOptions(..)
  , defaultRequest
  , RegisterFields(..)
  , LoginFields(..)
  , AuthFieldsRep(..)
  , login
  , register
  ) where

import Prelude

import API.Endpoint (Endpoint(..), endpointCodec)
import Affjax (Request, printError, request)
import Affjax.RequestBody as RB
import Affjax.RequestHeader (RequestHeader(..))
import Affjax.ResponseFormat as RF
import Data.Argonaut.Core (Json)
import Data.Argonaut.Decode.Struct.Tolerant as Tolerant
import Data.Argonaut.Encode (encodeJson)
import Data.Bifunctor (bimap)
import Data.Either (Either(..))
import Data.HTTP.Method (Method(..))
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype)
import Data.Profile (Profile)
import Data.Tuple (Tuple(..))
import Data.Username (Username)
import Effect (Effect)
import Effect.Aff.Class (class MonadAff, liftAff)
import Routing.Duplex (print)
import Web.HTML (window)
import Web.HTML.Window (localStorage)
import Web.Storage.Storage (getItem, removeItem, setItem)


newtype BaseURL = BaseURL String
newtype WsURL = WsURL String

derive instance newtypeWsURL :: Newtype WsURL _

data RequestMethod
  = Get
  | Post (Maybe Json)
  | Put (Maybe Json)
  | Delete

type RequestOptions =
  { endpoint :: Endpoint
  , method :: RequestMethod
  }

defaultRequest :: BaseURL -> Boolean -> RequestOptions -> Request Json
defaultRequest (BaseURL baseUrl) auth { endpoint, method } =
  { method: Left method
  , url: baseUrl <> print endpointCodec endpoint
    -- TODO: add X-XSRF-TOKEN header
  , headers: []
  , content: RB.json <$> body
  , username: Nothing
  , password: Nothing
  , withCredentials: auth
  , responseFormat: RF.json
  }
  where
  Tuple method body = case method of
    Get -> Tuple GET Nothing
    Post b -> Tuple POST b
    Put b -> Tuple PUT b
    Delete -> Tuple DELETE Nothing


type AuthFieldsRep r = ( username :: Username, password :: String | r )

type RegisterFields = { | AuthFieldsRep () }
type LoginFields = { | AuthFieldsRep () }

-- | This function logs a user in (if they exist), returning an auth token and the user's
-- | minimal profile.
login :: forall m. MonadAff m => BaseURL -> LoginFields -> m (Either String Profile)
login baseUrl fields =
  let method = Post $ Just $ encodeJson fields
   in requestUser baseUrl { endpoint: Login, method }

-- | This function registers a user (if they don't already exist), returning an auth token and the
-- | user's minimal profile.
register :: forall m. MonadAff m => BaseURL -> RegisterFields -> m (Either String Profile)
register baseUrl fields =
  -- let method = Post $ Just $ encodeJson { user: fields }
  let method = Post $ Just $ encodeJson fields
   in requestUser baseUrl { endpoint: Register, method }

-- | The login and registration requests share the same underlying implementation, just a different
-- | endpoint. This function can be re-used by both requests.
requestUser :: forall m. MonadAff m => BaseURL -> RequestOptions -> m (Either String Profile)
requestUser baseUrl opts = do
  res <- liftAff $ request $ defaultRequest baseUrl false opts
  pure $ Tolerant.decodeJson =<<  bimap printError _.body res

