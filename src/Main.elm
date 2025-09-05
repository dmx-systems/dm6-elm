module Main exposing (Flags, Model, Msg, init, main, subscriptions, update, view)

import AppModel as AM
import Browser
import Html exposing (text)
import Json.Decode as D
import Storage exposing (modelDecoder)



-- Reuse canonical types


type alias Model =
    AM.Model


type alias Msg =
    AM.Msg



-- Flags from JS bootstrap


type alias Flags =
    { slug : String
    , stored : String
    }



-- Lenient loader: try Storage.modelDecoder, else fall back to AM.default


decodeOrDefault : String -> Model
decodeOrDefault raw =
    case D.decodeString modelDecoder raw of
        Ok m ->
            m

        Err _ ->
            AM.default



-- INIT


init : Flags -> ( Model, Cmd Msg )
init flags =
    let
        m0 =
            decodeOrDefault flags.stored
    in
    ( m0, Cmd.none )



-- UPDATE


update : Msg -> Model -> ( Model, Cmd Msg )
update _ model =
    ( model, Cmd.none )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none



-- VIEW


view : Model -> Browser.Document Msg
view _ =
    { title = "DM6 Elm"
    , body = [ text "" ]
    }



-- MAIN


main : Program Flags Model Msg
main =
    Browser.document
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }
