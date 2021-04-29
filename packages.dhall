let upstream =
      https://github.com/purescript/package-sets/releases/download/psc-0.14.1-20210427/packages.dhall sha256:edbb8f70232fb83895c7ce02f5d2b29f6ee1722f1a70fc58d3bc0ab0de18afe4

in  upstream
  with halogen-select =
    { repo = "https://github.com/citizennet/purescript-halogen-select"
    , version = "v6.0.0"
    , dependencies = [ "halogen", "record" ]
    }
