let upstream =
      https://github.com/purescript/package-sets/releases/download/psc-0.14.0-20210329/packages.dhall sha256:32c90bbcd8c1018126be586097f05266b391f6aea9125cf10fba2292cb2b8c73

in  upstream
  with halogen-select =
    { repo = "https://github.com/PureFunctor/purescript-halogen-select"
    , dependencies =
      [ "halogen"
      , "halogen-hooks"
      , "halogen-hooks-extra"
      ]
    , version = "master"
    }

  with halogen-storybook =
    { repo = "https://github.com/rnons/purescript-halogen-storybook"
    , dependencies =
      [ "console"
      , "debug"
      , "effect"
      , "foreign-object"
      , "halogen"
      , "psci-support"
      , "routing"
      ]
    , version = "v1.0.1"
    }

