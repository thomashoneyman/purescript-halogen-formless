let conf = ../spago.dhall
in conf //
  { dependencies =
      conf.dependencies #
        [ "arrays"
        , "console"
        , "dom-indexed"
        , "effect"
        , "foreign-object"
        , "halogen-select"
        , "halogen-storybook"
        , "integers"
        , "strings"
        , "web-events"
        , "web-uievents"
        ]
  , sources =
      conf.sources #
        [ "example/**/*.purs"
        ]
  }
