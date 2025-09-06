module Logger exposing (..)



--- PROD LOGGER


log : String -> a -> a
log _ val = val


toString : a -> String
toString _ = ""
