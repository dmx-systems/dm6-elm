module Log.Prod exposing (debug, info, log, warn, withConsole)


info : String -> a -> a
info _ v =
    v


warn : String -> a -> a
warn _ v =
    v


debug : String -> a -> a
debug _ v =
    v


withConsole : String -> a -> a
withConsole _ v =
    v


log : String -> a -> a
log =
    debug
