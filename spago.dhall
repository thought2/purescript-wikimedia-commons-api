{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name =
    "my-project"
, dependencies =
    [ "effect"
    , "console"
    , "psci-support"
    , "sized-vectors"
    , "typelevel"
    , "tuples"
    , "affjax"
    , "argonaut"
    , "aff"
    , "string-parsers"
    , "generics-rep"
    ]
, packages =
    ./packages.dhall
, sources =
    [ "src/**/*.purs", "test/**/*.purs" ]
}
