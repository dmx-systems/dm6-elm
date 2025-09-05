port module AppMain exposing (Model, Msg, init, main, subscriptions, update, view)

import Browser exposing (Document)
import Json.Decode as D
import Main


port persist : String -> Cmd msg


type alias Model =
    Main.Model


type alias Msg =
    Main.Msg


init : D.Value -> ( Model, Cmd Msg )
init flagsVal =
    let
        flagsDecoder : D.Decoder Main.Flags
        flagsDecoder =
            D.map2 Main.Flags
                (D.field "slug" D.string)
                (D.field "stored" D.string)

        coldBoot : ( Model, Cmd Msg )
        coldBoot =
            Main.init { slug = "dm6-elm-demo", stored = "{}" }
    in
    case D.decodeValue flagsDecoder flagsVal of
        Ok flags ->
            -- New path with explicit {slug,stored}
            Main.init flags

        Err _ ->
            -- Anything else (legacy full-model JSON, {}, null, numbers, etc.) → cold boot
            coldBoot


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    Main.update msg model


subscriptions : Model -> Sub Msg
subscriptions =
    Main.subscriptions


view : Model -> Document Msg
view =
    Main.view



-- Accept Json.Value so {} and null are valid and legacy callers don't crash


main : Program D.Value Model Msg
main =
    Browser.document
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }
