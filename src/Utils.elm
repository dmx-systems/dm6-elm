module Utils exposing (..)

import Html exposing (Html, Attribute, text, br)
import Html.Events exposing (on, stopPropagationOn, keyCode)
import Json.Decode as D
import Debug exposing (log, toString)
{--
log : String -> a -> a
log text val = val

toString : a -> String
toString val = ""
--}



-- GENRAL ELM UTILITIES


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
  log ("### ERROR @" ++ funcName ++ ": " ++ text) val


fail : String -> a -> v -> v
fail funcName args val =
  log ("--> @" ++ funcName ++ " failed " ++ toString args) val


call : String -> a -> v -> v
call funcName args val =
  log ("@" ++ funcName ++ " " ++ toString args ++ " -->") val


info : String -> v -> v
info funcName val =
  log ("@" ++ funcName) val
