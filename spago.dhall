{ name = "halogen-realworld"
, packages = ./packages.dhall
, dependencies =
  [ "aff"
  , "aff-bus"
  , "aff-promise"
  , "affjax"
  , "argonaut-core"
  , "codec-argonaut"
  , "console"
  , "datetime"
  , "debug"
  , "effect"
  , "filterable"
  , "formatters"
  , "halogen"
  , "halogen-formless"
  , "halogen-select"
  , "jwt"
  , "nonempty"
  , "now"
  , "nullable"
  , "precise-datetime"
  , "prelude"
  , "remotedata"
  , "routing"
  , "routing-duplex"
  , "slug"
  , "strings"
  , "variant"
  ]
, sources =
  [ "src/**/*.purs"
  , "${      if (env:PRODUCTION : Bool) ? False

       then  "env/prod/*.purs"

       else  "env/dev/*.purs"}"
  , "test/**/*.purs"
  ]
}
