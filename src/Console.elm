module Console exposing (..)

import ModelBase exposing (..)

import Logger
import String exposing (fromInt)



topicNotFound : String -> TopicId -> a -> a
topicNotFound funcName id val =
  notFound funcName "Topic" (toTopicId id) val


assocNotFound : String -> AssocId -> a -> a
assocNotFound funcName id val =
  notFound funcName "Assoc" (toAssocId id) val


boxNotFound : String -> BoxId -> a -> a
boxNotFound funcName id val =
  notFound funcName "Box" (toBoxId id) val


itemSetNotFound : String -> Id -> a -> a
itemSetNotFound funcName id val =
  notFound funcName "ItemSet" id val


notFound : String -> String -> Id -> a -> a
notFound funcName item id val =
  logError funcName (item ++ " " ++ fromInt id ++ " not found") val


--

logError : String -> String -> v -> v
logError funcName text val =
  Logger.log ("❌ @" ++ funcName ++ ": " ++ text) val


fail : String -> a -> v -> v
fail funcName args val =
  Logger.log ("--> @" ++ funcName ++ ": " ++ Logger.toString args ++ " failed") val


info : String -> v -> v
info funcName val =
  Logger.log ("@" ++ funcName) val


toString : a -> String
toString = Logger.toString
