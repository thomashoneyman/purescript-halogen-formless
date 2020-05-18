{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name = "halogen-formless"
, dependencies =
  [ "console"
  , "debug"
  , "effect"
  , "generics-rep"
  , "halogen"
  , "halogen-hooks-extra"
  , "halogen-select"
  , "halogen-storybook"
  , "heterogeneous"
  , "profunctor-lenses"
  , "psci-support"
  , "variant"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
