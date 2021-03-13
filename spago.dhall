{ name = "dynamodb-singletable"
, dependencies =
  [ "aff"
  , "aff-promise"
  , "console"
  , "debug"
  , "effect"
  , "foreign"
  , "foreign-object"
  , "formatters"
  , "profunctor-lenses"
  , "record"
  , "rio"
  , "psci-support"
  , "spec"
  , "untagged-union"
  , "variant"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
