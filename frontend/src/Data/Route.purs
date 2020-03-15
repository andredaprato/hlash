module Data.Route where

import Prelude

import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Routing.Duplex (RouteDuplex', parse, path, root, segment, string)
import Routing.Duplex.Generic as G
import Routing.Duplex.Generic.Syntax ((/))
import Routing.Hash (matches, matchesWith)
import Routing.Match (Match, lit, num, str, end)

data Route  = Login
            | Register
            | Home 

derive instance genericRoutes :: Generic Route _ 
derive instance eqRoute :: Eq Route
derive instance ordRoute :: Ord Route
instance showRoutes :: Show Route where
  show = genericShow
         

router :: RouteDuplex' Route
router =  root $ G.sum
  { "Login" : "login" / G.noArgs
  , "Register" : "register" / G.noArgs
  , "Home" : "home" / G.noArgs
  }
