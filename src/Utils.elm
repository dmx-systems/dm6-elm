module Utils exposing (..)

import ModelParts exposing (..)

import Html exposing (Attribute)
import Html.Events exposing (on, stopPropagationOn, keyCode)
import Json.Decode as D
import Logger
import String exposing (fromInt)
import Task



-- EVENTS


onEsc : msg -> Attribute msg
onEsc msg =
  on "keydown" (keyDecoder 27 msg)


onEnterOrEsc : msg -> Attribute msg
onEnterOrEsc msg =
  on "keydown"
    ( D.oneOf
      [ keyDecoder 13 msg
      , keyDecoder 27 msg
      ]
    )


keyDecoder : Int -> msg -> D.Decoder msg
keyDecoder key msg =
  let
    isKey code =
      if code == key then
        D.succeed msg
      else
        D.fail "not that key"
  in
  keyCode |> D.andThen isKey


onClickStop : msg -> Attribute msg
onClickStop msg =
  stopPropagation "click" msg


onMouseDownStop : msg -> Attribute msg
onMouseDownStop msg =
  stopPropagation "mousedown" msg


onMouseOverStop : msg -> Attribute msg
onMouseOverStop msg =
  stopPropagation "mouseover" msg


onMouseOutStop : msg -> Attribute msg
onMouseOutStop msg =
  stopPropagation "mouseout" msg


stopPropagation : String -> msg -> Attribute msg
stopPropagation eventName msg =
  stopPropagationOn eventName <| D.succeed (msg, True)



-- DECODER


pointDecoder : D.Decoder Point
pointDecoder =
  D.map2 Point
    (D.field "clientX" D.float |> D.andThen toIntDecoder)
    (D.field "clientY" D.float |> D.andThen toIntDecoder)


toIntDecoder : Float -> D.Decoder Int
toIntDecoder float =
  round float |> D.succeed



-- COMMAND


command : msg -> Cmd msg
command msg =
  Task.succeed () |> Task.perform (\_ -> msg)



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


info : String -> v -> v
info funcName val =
  Logger.log ("@" ++ funcName) val


toString : a -> String
toString = Logger.toString
