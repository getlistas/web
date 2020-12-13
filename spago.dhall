{ name = "halogen-realworld"
, packages = ./packages.dhall
, dependencies =
  [ "aff"
  , "aff-bus"
  , "affjax"
  , "argonaut-core"
  , "codec-argonaut"
  , "console"
  , "debug"
  , "effect"
  , "formatters"
  , "halogen"
  , "halogen-formless"
  , "jwt"
  , "nonempty"
  , "now"
  , "precise-datetime"
  , "prelude"
  , "remotedata"
  , "routing"
  , "routing-duplex"
  , "slug"
  , "variant"
  ]
, sources =
  [ "src/**/*.purs"
  -- https://discourse.purescript.org/t/how-do-you-read-node-env-while-bundling/1743/3
  , "${if (((env:PRODUCTION : Bool) ? False )) then "env/prod/*.purs" else "env/dev/*.purs"}"
  , "test/**/*.purs"
  ]
}
