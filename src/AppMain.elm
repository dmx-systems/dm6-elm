port module AppMain exposing
    ( Flags
    , Model
    , Msg
    , init
    , main
    , subscriptions
    , update
    , view
    )

import Browser exposing (Document)
import Main



-- Ports


port persist : String -> Cmd msg



-- Types


type alias Model =
    Main.Model


type alias Msg =
    Main.Msg


type alias Flags =
    Main.Flags


init : Flags -> ( Model, Cmd Msg )
init flags =
    let
        ( model, cmd ) =
            Main.init flags
    in
    ( model, Cmd.map identity cmd )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    Main.update msg model


subscriptions : Model -> Sub Msg
subscriptions =
    Main.subscriptions


view : Model -> Document Msg
view =
    Main.view


main : Program Flags Model Msg
main =
    Browser.document
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }
