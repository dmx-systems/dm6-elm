module Main exposing (..)

import Model exposing (..)
import Browser exposing (Document)
import Html exposing (text)



-- MAIN


main =
  Browser.document
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions
    }


init : () -> ( Model, Cmd Msg )
init flags =
  ( Model
  , Cmd.none
  )



-- VIEW


view : Model -> Document Msg
view model =
  Document
    "Elm Starter"
    [ text "Elm Starter"
    ]



-- UPDATE


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
  (model, Cmd.none)



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.none
