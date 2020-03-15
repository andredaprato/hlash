module Page.DisplayMembers where

import Prelude

import Data.Maybe (Maybe(..))
import Data.Profile (Profile)
import Data.Username (toString)
import Halogen as H
import Halogen.HTML as HH
import UI.UI (column, css)

renderMembers :: forall act slots m . Array (Maybe Profile) -> H.ComponentHTML act slots m
renderMembers members =
  HH.div [ css "section" ]
   [
     HH.div [ css "box" ]
     [
       HH.label [ css "label" ] [ HH.text "Players"]
     , HH.div [ css "columns is-centered is-multiline" ] $
       flip map members $ \member ->
       column "" 
       [
         case member of
               Nothing ->
                 HH.span [ css "tag is-medium is-warning" ]
                 [
                   HH.text "Waiting for players.."
                 ]

               Just m ->
                 HH.span [ css "tag is-medium is-info" ]
                 [
                   HH.text $ toString m.username
                 ]
       ]
     ]
   ]
