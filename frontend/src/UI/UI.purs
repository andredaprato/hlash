module UI.UI where

-- a module for  html element functions with styling already added

import Prelude

import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP


css :: forall r t. String -> HP.IProp ("class" :: String | r) t
css = HP.class_  <<< HH.ClassName

column modifier = HH.div [ css $ "column " <> modifier ]
