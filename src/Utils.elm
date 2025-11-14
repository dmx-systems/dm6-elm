module Utils exposing (..)

import Model exposing (..)

import Html exposing (Html, Attribute, text, br)
import Html.Events exposing (on, stopPropagationOn, keyCode)
import Json.Decode as D
import Logger


-- Events

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


-- Decoder

classDecoder : D.Decoder Class
classDecoder =
  D.oneOf
    [ D.at ["target", "className"] D.string -- HTML elements
    , D.at ["target", "className", "baseVal"] D.string -- SVG elements
    ]


idDecoder : D.Decoder Id
idDecoder =
  D.at ["target", "dataset", "id"] D.string
  |> D.andThen toIntDecoder


idTupleDecoder : D.Decoder (Id, Id)
idTupleDecoder =
  D.at ["target", "dataset", "id"] D.string
  |> D.andThen toIntTupleDecoder


pathDecoder : D.Decoder BoxPath
pathDecoder =
  D.at ["target", "dataset", "path"] D.string
  |> D.andThen toIntListDecoder


pointDecoder : D.Decoder Point
pointDecoder =
  D.map2 Point
    (D.field "clientX" D.float)
    (D.field "clientY" D.float)


--

toIntDecoder : String -> D.Decoder Int
toIntDecoder str =
  case String.toInt str of
    Just int -> D.succeed int
    Nothing -> D.fail <| "\"" ++ str ++ "\" is not an Int"


toIntTupleDecoder : String -> D.Decoder (Int, Int)
toIntTupleDecoder string =
  D.succeed <|
    case toIntList string of
      [int1, int2] -> (int1, int2)
      _ -> logError "toIntTupleDecoder" ("\"" ++ string ++ "\" is not a pair") (-1, -1)


toIntListDecoder : String -> D.Decoder (List Int)
toIntListDecoder string =
  D.succeed <| toIntList string


toIntList : String -> List Int
toIntList string =
  string |> String.split "," |> List.map
    (\str ->
      case str |> String.toInt of
        Just int -> int
        Nothing -> logError "toIntList" ("\"" ++ str ++ "\" is not an Int") -1
    )


-- HTML

multilineHtml : String -> List (Html msg)
multilineHtml str =
  String.lines str |> List.foldr
    (\line linesAcc ->
      [ text line, br [] [] ] ++ linesAcc
    )
    []


-- Debug

logError : String -> String -> v -> v
logError funcName text val =
  Logger.log ("### ERROR @" ++ funcName ++ ": " ++ text) val


fail : String -> a -> v -> v
fail funcName args val =
  Logger.log ("--> @" ++ funcName ++ " failed " ++ Logger.toString args) val


call : String -> a -> v -> v
call funcName args val =
  Logger.log ("@" ++ funcName ++ " " ++ Logger.toString args ++ " -->") val


info : String -> v -> v
info funcName val =
  Logger.log ("@" ++ funcName) val


toString : a -> String
toString = Logger.toString
