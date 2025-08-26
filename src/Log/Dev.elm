module Log.Dev exposing (debug, info, log, warn, withConsole)

import Debug

info : String -> a -> a
info label v =
    Debug.log ("ℹ️ " ++ label) v

warn : String -> a -> a
warn label v =
    Debug.log ("⚠️ " ++ label) v

debug : String -> a -> a
debug label v =
    Debug.log ("🐛 " ++ label) v

-- Log a message without dumping huge values (like the model).
-- Works in pipelines:  foo |> withConsole "clicked"
withConsole : String -> a -> a
withConsole message v =
    let
        _ = Debug.log message ()
    in
    v

-- Back-compat
log : String -> a -> a
log =
    debug
