let conf = ../spago.dhall
in conf //
  { dependencies =
      conf.dependencies #
        [ "debug"
        , "halogen-select"
        , "halogen-storybook"
        , "profunctor-lenses"
        ]
  , sources =
      conf.sources #
        [ "example/**/*.purs"
        ]
  }
