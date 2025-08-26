module Utils exposing (..)

-- your pass-through, pipe-style logger (no-op in prod)

import Html exposing (Attribute, Html, br, text)
import Html.Events exposing (keyCode, on, stopPropagationOn)
import Json.Decode as D
import Log exposing (log)
import Platform.Cmd as Cmd exposing (Cmd)
import Ports.Console as Console



-- tiny console port used for real logs in prod
-- If you only used `toString` for logging, make it a no-op to avoid Debug.toString.
-- (Elm has no generic to-string without Debug.)


toString : a -> String
toString _ =
    ""



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



-- Debug-ish helpers (pipe-style, remain pure: no console in prod)


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



-- Effectful companions for real console output (work under --optimize)


infoCmd : String -> Cmd msg
infoCmd =
    Console.log


withInfo : String -> ( model, Cmd msg ) -> ( model, Cmd msg )
withInfo msg ( m, c ) =
    ( m, Cmd.batch [ c, infoCmd msg ] )


maybeInfo : Maybe String -> ( model, Cmd msg ) -> ( model, Cmd msg )
maybeInfo m tuple =
    case m of
        Just s ->
            withInfo s tuple

        Nothing ->
            tuple


withConsole : String -> ( model, Cmd msg ) -> ( model, Cmd msg )
withConsole line ( model, cmd ) =
    ( model, Cmd.batch [ cmd, Console.log line ] )
