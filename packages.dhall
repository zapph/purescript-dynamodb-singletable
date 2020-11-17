let upstream =
      https://github.com/purescript/package-sets/releases/download/psc-0.13.8-20200911-2/packages.dhall sha256:872c06349ed9c8210be43982dc6466c2ca7c5c441129826bcb9bf3672938f16e

let overrides = {=}

let additions =
  { oneof =
      { dependencies =
        [ "assert"
        , "console"
        , "effect"
        , "foreign"
        , "literal"
        , "maybe"
        , "proxy"
        , "psci-support"
        , "tuples"
        , "unsafe-coerce"
        ]
      , repo = "https://github.com/jvliwanag/purescript-oneof.git"
      , version = "0325fddf6ee8a181fac2128c9b542c2c01ddd361"
      }
  , literal =
      { dependencies =
        [ "assert"
        , "effect"
        , "console"
        , "integers"
        , "numbers"
        , "partial"
        , "psci-support"
        , "unsafe-coerce"
        , "typelevel-prelude"
        ]
      , repo = "https://github.com/jvliwanag/purescript-literal.git"
      , version = "7b2ae20f77c67b7e419a92fdd0dc7a09b447b18e"
      }
  , rio =
      { dependencies =
        [ "aff"
        , "console"
        , "effect"
        , "psci-support"
        , "transformers"
        ]
      , repo = "https://github.com/zapph/purescript-rio.git"
      , version = "3da1f6c26d00affb9106967bdcf6443278347843"
      }

  }

in  upstream // overrides // additions