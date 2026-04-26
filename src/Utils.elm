module Utils exposing (..)

import ModelBase exposing (Id, Point, PointerType)

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


onPointerDownStop : msg -> Attribute msg
onPointerDownStop msg =
  stopPropagation "pointerdown" msg


onPointerOverStop : msg -> Attribute msg
onPointerOverStop msg =
  stopPropagation "pointerover" msg


onPointerOutStop : msg -> Attribute msg
onPointerOutStop msg =
  stopPropagation "pointerout" msg


stopPropagation : String -> msg -> Attribute msg
stopPropagation eventName msg =
  stopPropagationOn eventName <| D.succeed (msg, True) -- stopPropagation=True


stopPropagationWith : String -> D.Decoder msg -> Attribute msg
stopPropagationWith eventName decoder =
  stopPropagationOn eventName
    ( decoder
        |> D.map (\msg -> (msg, True)) -- stopPropagation=True
    )



-- DECODER


pointDecoder : D.Decoder (Point, PointerType)
pointDecoder =
  D.map2 Tuple.pair
    ( D.map2 Point
        (D.field "clientX" D.float |> D.map round)
        (D.field "clientY" D.float |> D.map round)
    )
    ( D.field "pointerType" D.string )



-- COMMAND


command : msg -> Cmd msg
command msg =
  Task.succeed () |> Task.perform (\_ -> msg)



-- DEBUG


topicNotFound : String -> Id -> a -> a
topicNotFound funcName id val =
  notFound funcName "Topic" id val


assocNotFound : String -> Id -> a -> a
assocNotFound funcName id val =
  notFound funcName "Assoc" id val


boxNotFound : String -> Id -> a -> a
boxNotFound funcName id val =
  notFound funcName "Box" id val


itemSetNotFound : String -> Id -> a -> a
itemSetNotFound funcName id val =
  notFound funcName "ItemSet" id val


notFound : String -> String -> Id -> a -> a
notFound funcName item id val =
  logError funcName (item ++ " " ++ fromInt id ++ " not found") val


itemNotInBox : String -> Id -> Id -> a -> a
itemNotInBox funcName itemId boxId val =
  logError funcName ("item " ++ fromInt itemId ++ " not in box " ++ fromInt boxId) val


--

logError : String -> String -> v -> v
logError funcName text val =
  Logger.log ("💥 @" ++ funcName ++ ": " ++ text) val


fail : String -> a -> v -> v
fail funcName args val =
  Logger.log ("--> @" ++ funcName ++ " " ++ Logger.toString args ++ " failed") val


info : String -> v -> v
info funcName val =
  Logger.log ("@" ++ funcName) val


toString : a -> String
toString = Logger.toString
