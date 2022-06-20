{ name = "halogen-project"
, dependencies =
  [ "aff"
  , "aff-promise"
  , "arrays"
  , "console"
  , "control"
  , "effect"
  , "either"
  , "exceptions"
  , "foldable-traversable"
  , "halogen"
  , "lists"
  , "maybe"
  , "newtype"
  , "parsing"
  , "prelude"
  , "psci-support"
  , "random"
  , "spec"
  , "strings"
  , "transformers"
  , "tuples"
  , "web-dom"
  , "web-html"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
