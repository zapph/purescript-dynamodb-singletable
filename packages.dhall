let upstream =
      https://github.com/purescript/package-sets/releases/download/psc-0.14.0-20210311/packages.dhall sha256:3da8be2b7b4a0e7de6186591167b363023695accffb98a8639e9e7d06e2070d6

let overrides = {=}

let additions =
      { rio =
        { dependencies =
          [ "aff", "console", "effect", "psci-support", "transformers" ]
        , repo = "https://github.com/zapph/purescript-rio.git"
        , version = "3da1f6c26d00affb9106967bdcf6443278347843"
        }
      }

in  upstream ⫽ overrides ⫽ additions
