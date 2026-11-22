module Console exposing (..)

import Logger



-- log a root cause
logError : String -> String -> v -> v
logError funcName text val =
  Logger.log ("❌ @" ++ funcName ++ ": " ++ text) val


-- log a consequence
fail : String -> a -> v -> v
fail funcName args val =
  Logger.log ("🔺 @" ++ funcName ++ " failed: " ++ Logger.toString args) val


info : String -> v -> v
info funcName val =
  Logger.log ("@" ++ funcName) val


infoGroup : v -> v
infoGroup val =
  Logger.log "🔷" val


toString : a -> String
toString = Logger.toString
