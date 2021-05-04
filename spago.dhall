{ name = "halogen-realworld"
, packages = ./packages.dhall
, dependencies =
  [ "aff"
  , "aff-bus"
  , "aff-promise"
  , "affjax"
  , "argonaut-codecs"
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
  , "integers"
  , "jwt"
  , "nonempty"
  , "now"
  , "nullable"
  , "orders"
  , "precise-datetime"
  , "prelude"
  , "profunctor-lenses"
  , "remotedata"
  , "routing"
  , "routing-duplex"
  , "strings"
  , "svg-parser-halogen"
  , "unsafe-coerce"
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
