{ name = "formless"
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
, packages = ./packages.dhall
, sources = [ "src/**/*.purs" ]
}
