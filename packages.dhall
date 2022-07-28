let upstream =
      https://github.com/purescript/package-sets/releases/download/psc-0.15.2-20220706/packages.dhall
        sha256:7a24ebdbacb2bfa27b2fc6ce3da96f048093d64e54369965a2a7b5d9892b6031

let overrides = {=}

let additions =
      { subcategory =
        { dependencies = [ "prelude", "profunctor", "record" ]
        , repo = "https://github.com/matthew-hilty/purescript-subcategory.git"
        , version = "v0.2.0"
        }
      }

in  (upstream // overrides // additions)
  with variant.version = "map-variant"
  with variant.repo = "https://github.com/MonoidMusician/purescript-variant"

  with convertable-options =
    { repo = "https://github.com/natefaubion/purescript-convertable-options"
    , version = "v1.0.0"
    , dependencies = [ "console", "effect", "maybe", "record" ]
    }

  with halogen-select =
    { repo = "https://github.com/citizennet/purescript-halogen-select"
    , version = "v5.0.0"
    , dependencies = [ "halogen", "record" ]
    }

  with svg-parser-halogen =
    { repo = "https://github.com/rnons/purescript-svg-parser-halogen"
    , version = "master"
    , dependencies =
        [ "arrays"
        , "bifunctors"
        , "either"
        , "halogen"
        , "prelude"
        , "svg-parser"
        ]
    }

  with svg-parser =
    { repo = "https://github.com/rnons/purescript-svg-parser"
    , version = "master"
    , dependencies =
        [ "arrays"
        , "control"
        , "either"
        , "lists"
        , "prelude"
        , "string-parsers"
        , "strings"
        ]
    }

  with halogen-formless =
    { repo = "https://github.com/thomashoneyman/purescript-halogen-formless"
    , version = "main"
    , dependencies =
      [ "convertable-options"
      , "effect"
      , "either"
      , "foldable-traversable"
      , "foreign-object"
      , "halogen"
      , "heterogeneous"
      , "maybe"
      , "prelude"
      , "record"
      , "safe-coerce"
      , "type-equality"
      , "unsafe-coerce"
      , "unsafe-reference"
      , "variant"
      , "web-events"
      , "web-uievents"
      ]
    }

