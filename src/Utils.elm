module Utils exposing (..)

import ModelParts exposing (..)

import Html exposing (Attribute)
import Html.Events exposing (on, stopPropagationOn, keyCode)
import Json.Decode as D
import Logger
import String exposing (fromInt)



-- EVENTS


onEsc : msg -> Attribute msg
onEsc msg_ =
  on "keydown" (keyDecoder 27 msg_)


onEnterOrEsc : msg -> Attribute msg
onEnterOrEsc msg_ =
  on "keydown"
    ( D.oneOf
      [ keyDecoder 13 msg_
      , keyDecoder 27 msg_
      ]
    )


keyDecoder : Int -> msg -> D.Decoder msg
keyDecoder key msg_ =
  let
    isKey code =
      if code == key then
        D.succeed msg_
      else
        D.fail "not that key"
  in
  keyCode |> D.andThen isKey


stopPropagationOnMousedown : msg -> Attribute msg
stopPropagationOnMousedown msg_ =
  stopPropagationOn "mousedown" <| D.succeed (msg_, True)



-- DECODER


pointDecoder : D.Decoder Point
pointDecoder =
  D.map2 Point
    (D.field "clientX" D.float)
    (D.field "clientY" D.float)



-- DEBUG


itemNotInBox : String -> Id -> Id -> a -> a
itemNotInBox funcName itemId boxId val =
  logError funcName ("item " ++ fromInt itemId ++ " not in box " ++ fromInt boxId) val


topicMismatch : String -> Id -> a -> a
topicMismatch funcName id val =
  logError funcName (fromInt id ++ " is not a Topic but an Assoc") val


assocMismatch : String -> Id -> a -> a
assocMismatch funcName id val =
  logError funcName (fromInt id ++ " is not an Assoc but a Topic") val


illegalBoxId : String -> Id -> a -> a
illegalBoxId funcName id val =
  illegalId funcName "Box" id val


illegalItemId : String -> Id -> a -> a
illegalItemId funcName id val =
  illegalId funcName "Item" id val


illegalId : String -> String -> Id -> a -> a
illegalId funcName item id val =
  logError funcName (fromInt id ++ " is an illegal " ++ item ++ " ID") val


--

logError : String -> String -> v -> v
logError funcName text val =
  Logger.log ("### ERROR @" ++ funcName ++ ": " ++ text) val


fail : String -> a -> v -> v
fail funcName args val =
  Logger.log ("--> @" ++ funcName ++ " " ++ Logger.toString args ++ " failed") val


call : String -> a -> v -> v
call funcName args val =
  Logger.log ("@" ++ funcName ++ " " ++ Logger.toString args ++ " -->") val


info : String -> v -> v
info funcName val =
  Logger.log ("@" ++ funcName) val


toString : a -> String
toString = Logger.toString
