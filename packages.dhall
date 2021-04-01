let upstream =
      https://github.com/purescript/package-sets/releases/download/psc-0.14.0-20210331/packages.dhall sha256:fe3b63fe4b0cd1518c0ee506751b5b16d2c47210df94b5beb48be6570fe7f78a

in  upstream
  with halogen-select =
    { repo = "https://github.com/citizennet/purescript-halogen-select"
    , version = "v6.0.0"
    , dependencies = [ "halogen", "record" ]
    }
