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


withConsole : String -> a -> a
withConsole _ v =
    v


log : String -> a -> a
log =
    debug
