{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name = "my-project"
, dependencies =
  [ "effect"
  , "console"
  , "psci-support"
  , "halogen"
  , "affjax"
  , "routing"
  , "newtype"
  , "halogen-vdom"
  , "halogen-formless"
  , "argonaut"
  , "tolerant-argonaut"
  , "aff-bus"
  , "argonaut-generic"
  , "milkis"
  , "routing"
  , "routing-duplex"
  , "websocket-simple"
  , "aff-coroutines"
  , "unicode"
  , "js-timers"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
