module API.Endpoint where

import Prelude

import Control.Alternative ((<|>))
import Data.Const (Const(..))
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Maybe (Maybe(..))
import Data.String (toLower)
import Data.Symbol (SProxy(..))
import Effect (Effect)
import Effect.Aff (Aff, launchAff)
import Effect.Class (liftEffect)
import Effect.Class.Console (log)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Routing.Duplex (RouteDuplex', parse, path, root, segment, string, optional, int)
import Routing.Duplex.Generic as G
import Routing.Duplex.Generic.Syntax ((/), (?))
import Routing.Hash (matches, matchesWith)
import UI.UI (css)

type Pagination = {limit :: Maybe Int}



data Endpoint  = Login
               | Register
               | Logout
               | IsLoggedIn
               | GetQuestion
               | MakeQuestion 
               | JoinLobby
               | CreateLobby 

derive instance genericEndpoint :: Generic Endpoint _ 

instance showEndpoint :: Show Endpoint where
  show = genericShow
         

endpointCodec :: RouteDuplex' Endpoint
endpointCodec =  root $ G.sum
  { "Login" : "login" / G.noArgs
  , "Register" : "register" / G.noArgs
  , "Logout" : "logout" / G.noArgs
  , "IsLoggedIn" : "isLoggedIn" / G.noArgs
  , "GetQuestion" : "question" / G.noArgs
  , "MakeQuestion" : "question" /  G.noArgs
  , "JoinLobby" : "lobby" / "join" / G.noArgs
  , "CreateLobby" : "lobby" / "create" / G.noArgs

  }
