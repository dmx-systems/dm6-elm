module Logger exposing (..)

import Debug



--- DEV LOGGER


log : String -> a -> a
log = Debug.log


toString : a -> String
toString = Debug.toString
