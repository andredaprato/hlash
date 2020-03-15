module UI.Form where

import Prelude

import DOM.HTML.Indexed (HTMLinput)
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype)
import Data.Symbol (class IsSymbol, SProxy(..))
import Data.Variant (Variant)
import Formless as F
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Type.Row as Row
import UI.UI (css)

input 
  :: forall form act slots m sym fields inputs out formError t0 t1
     . IsSymbol sym
     => Newtype (form Record F.FormField) { | fields }
  => Newtype (form Variant F.InputFunction) (Variant inputs)
  => Row.Cons sym (F.FormField formError String out) t0 fields
  => Row.Cons sym (F.InputFunction formError String out) t1 inputs
  => SProxy sym
  -> form Record F.FormField
  -> Array (HH.IProp HTMLinput (F.Action form act))
  -> F.ComponentHTML form act slots m
input proxy form props = 
  HH.fieldset [css "form-group"] 
  [  HH.input $ 
      props <> [ HP.value $ F.getInput proxy form
      , HE.onValueInput $ Just <<< F.setValidate proxy
      , HP.type_ HP.InputText
      , css "input"
      ] 
     
  ]

control a = HH.div [ css "control"] [a]

field = HH.div [ css "field" ]
