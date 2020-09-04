{ name = "halogen-formless"
, dependencies = [ "halogen", "halogen-hooks", "record" ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
