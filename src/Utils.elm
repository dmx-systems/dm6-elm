module Utils exposing (..)

import Html exposing (Attribute, Html, br, text)
import Html.Events exposing (keyCode, on, stopPropagationOn)
import Json.Decode as D
import Log exposing (log)



{--
log : String -> a -> a
log text val = val
-}


toString : a -> String
toString =
    Log.toString



-- toString val = ""
-- GENRAL ELM UTILITIES
-- Events


onEsc : msg -> Attribute msg
onEsc msg_ =
    on "keydown" (keyDecoder 27 msg_)


onEnterOrEsc : msg -> Attribute msg
onEnterOrEsc msg_ =
    on "keydown"
        (D.oneOf
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


strToIntDecoder : String -> D.Decoder Int
strToIntDecoder str =
    case String.toInt str of
        Just int ->
            D.succeed int

        Nothing ->
            D.fail <| "\"" ++ str ++ "\" is an invalid ID"


stopPropagationOnMousedown : msg -> Attribute msg
stopPropagationOnMousedown msg_ =
    stopPropagationOn "mousedown" <| D.succeed ( msg_, True )



-- HTML


multilineHtml : String -> List (Html msg)
multilineHtml str =
    String.lines str
        |> List.foldr
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
