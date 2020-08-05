let conf = ../spago.dhall
in conf //
  { dependencies =
      conf.dependencies #
        [ "debug"
        , "halogen-select"
        , "halogen-storybook"
        ]
  , sources =
      conf.sources #
        [ "example/**/*.purs"
        ]
  }
