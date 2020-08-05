{ name = "halogen-formless"
, dependencies =
  [ "halogen"
  , "variant"
  , "heterogeneous"
  , "generics-rep"
  , "profunctor-lenses"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
