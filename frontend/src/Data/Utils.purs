module Data.Utils where

import Prelude

import Data.Argonaut.Decode.Generic.Rep (genericDecodeJsonWith)
import Data.Argonaut.Encode.Generic.Rep (genericEncodeJsonWith)
import Data.Argonaut.Types.Generic.Rep (defaultEncoding)

myGenericEncode = genericEncodeJsonWith $ defaultEncoding {valuesKey = "contents"
                                                          , unwrapSingleArguments = true
                                                          }

myGenericDecode = genericDecodeJsonWith $  defaultEncoding { unwrapSingleArguments = true
                                                           , valuesKey = "contents"
                                                           }

