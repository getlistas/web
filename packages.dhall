let upstream =
      https://github.com/purescript/package-sets/releases/download/psc-0.14.5-20220110/packages.dhall sha256:8dbf71bfc6c7a11043619eebe90ff85f7d884541048aa8cc48eef1ee781cbc0e

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
