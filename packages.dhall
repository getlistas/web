let upstream =
      https://github.com/purescript/package-sets/releases/download/psc-0.14.2-20210623/packages.dhall sha256:b3dcd9b0a1f6bffa4b5e61dc80e9f3fec2fca9405d591397a5076c8fe3aed1bc

let overrides = {=}

let additions =
      { subcategory =
        { dependencies = [ "prelude", "profunctor", "record" ]
        , repo = "https://github.com/matthew-hilty/purescript-subcategory.git"
        , version = "v0.2.0"
        }
      }

in  upstream // overrides // additions
