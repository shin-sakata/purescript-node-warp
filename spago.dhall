{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name = "my-project"
, dependencies =
  [ "console"
  , "effect"
  , "either"
  , "foreign-object"
  , "http-types"
  , "node-http"
  , "node-url"
  , "nullable"
  , "psci-support"
  , "wai"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
