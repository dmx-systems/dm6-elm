module Logger exposing (..)

import Debug



--- DEV LOGGER


toString : a -> String
toString =
  Debug.toString


log : String -> a -> a
log =
  Debug.log


todo : String -> a -> a
todo str _ =
  Debug.todo str
