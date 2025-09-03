module AppMain exposing
    ( Model
    , Msg
    , init
    , main
    , subscriptions
    , update
    , view
    )

import Browser exposing (Document)
import Html exposing (Html)
import Main



-- Re-export core types


type alias Model =
    Main.Model


type alias Msg =
    Main.Msg



-- Re-export core functions


init : () -> ( Model, Cmd Msg )
init =
    Main.init


update : Msg -> Model -> ( Model, Cmd Msg )
update =
    Main.update


subscriptions : Model -> Sub Msg
subscriptions =
    Main.subscriptions


view : Model -> Document Msg
view =
    Main.view


main : Program () Model Msg
main =
    Main.main
