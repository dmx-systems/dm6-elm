module Main exposing (..)

import Model exposing (..)
import Style exposing (..)

import Browser exposing (Document)
import Dict exposing (Dict)
import Html exposing (Html, div, text, button, h2)
import Html.Events exposing (onClick)



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
  ( Model Dict.empty 0
  , Cmd.none
  )



-- VIEW


view : Model -> Document Msg
view model =
  Document
    "Elm DM6"
    [ h2 [] [ text "Elm DM6" ]
    , button [ onClick AddTopic ] [ text "Add Topic" ]
    , viewGraph model
    ]


viewGraph : Model -> Html Msg
viewGraph model =
  div []
    ( Dict.values model.items |> List.map
      (\item ->
        case item of
          Topic id pos -> div (topicStyle pos) []
          Assoc _ _ _ _ -> text ""
      )
    )



-- UPDATE


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
  let
    id = model.nextId
  in
  ( { model
    | items = model.items |> Dict.insert id
      ( Topic id <| Point 92 64 )
    , nextId = id + 1
    }
    , Cmd.none
  )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.none
