module Log exposing
    ( error
    , info
    , log
    , toString
    , warn
    , withConsole
    )

{-| Production no-op logger. Same API as the dev version.
-}


log : String -> a -> a
log _ value =
    value


info : String -> a -> a
info _ value =
    value


warn : String -> a -> a
warn _ value =
    value


error : String -> a -> a
error _ value =
    value


withConsole : String -> ( model, Cmd msg ) -> ( model, Cmd msg )
withConsole _ tuple =
    tuple


toString : a -> String
toString _ =
    ""
