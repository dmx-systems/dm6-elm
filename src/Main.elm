module Main exposing (..)

import Model exposing (..)
import Style exposing (..)

import Browser
import Browser.Events as Events
import Dict exposing (Dict)
import Html exposing (Html, div, text, button, h2)
import Html.Attributes exposing (class, attribute)
import Html.Events exposing (onClick)
import Json.Decode as D
import Debug exposing (log)



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
      Dict.empty
      NoDrag
      0
  , Cmd.none
  )



-- VIEW


view : Model -> Browser.Document Msg
view model =
  Browser.Document
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
          Topic id pos -> viewTopic id pos
          Assoc _ _ _ _ -> text ""
      )
    )


viewTopic : Id -> Point -> Html Msg
viewTopic id pos =
  div
    ( [ class "dmx-topic"
      , attribute "data-id" (String.fromInt id)
      ]
      ++ topicStyle pos
    )
    []



-- UPDATE


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
  let
    _ = log "update" msg
  in
  case msg of
    AddTopic ->
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
    Mouse mouseMsg -> updateMouse mouseMsg model


updateMouse : MouseMsg -> Model -> ( Model, Cmd Msg )
updateMouse msg model =
  case msg of
    Down -> ( model, Cmd.none )
    DownItem class id pos -> ( mouseDownOnItem model class id pos, Cmd.none )
    Move pos -> ( mouseMove model pos, Cmd.none )
    Up -> mouseUp model


mouseDownOnItem : Model -> Class -> Id -> Point -> Model
mouseDownOnItem model class id pos =
  { model | dragState = DragEngaged class id pos }


mouseMove : Model -> Point -> Model
mouseMove model pos =
  case model.dragState of
    DragEngaged class id pos_ ->
      -- enter DragTopic state only on 1st move
      let
        dragState = case class of
          "dmx-topic" -> DragTopic id pos_
          _ -> NoDrag -- the error will be logged in performDrag
      in
      performDrag { model | dragState = dragState } pos
    DragTopic _ _ ->
      performDrag model pos
    NoDrag ->
      logError "mouseMove" "Received Move message when dragState is NoDrag" model


performDrag : Model -> Point -> Model
performDrag model pos =
  case model.dragState of
    DragTopic id lastPoint ->
      let
        delta = Point
          (pos.x - lastPoint.x)
          (pos.y - lastPoint.y)
      in
      { model
        | items = updateTopic model id delta
        , dragState = DragTopic id pos -- update lastPoint
      }
    DragEngaged _ _ _ ->
      logError "performDrag" "Received Move message when dragState is DragEngaged" model
    NoDrag ->
      logError "performDrag" "Received Move message when dragState is NoDrag" model


updateTopic : Model -> Id -> Delta -> Items
updateTopic model id delta =
  model.items |> Dict.update
    id
    (\item_ -> case item_ of
      Just (Topic id_ pos) -> Just
        (let
          pos_ = Point (pos.x + delta.x) (pos.y + delta.y)
        in
          Topic id_ pos_)
        -- { timespan | begin = timespan.begin + delta_ }
      Just assoc -> Just assoc
      Nothing -> illegalItemId "updateTopic" id Nothing
    )


mouseUp : Model -> ( Model, Cmd Msg )
mouseUp model =
  -- TODO
  ( { model | dragState = NoDrag }, Cmd.none )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
  case model.dragState of
    NoDrag -> mouseDownSub
    DragEngaged _ _ _ -> dragSub
    DragTopic _ _ -> dragSub


mouseDownSub : Sub Msg
mouseDownSub =
  Events.onMouseDown <| D.oneOf
    [ D.map Mouse <| D.map3 DownItem
        ( D.at ["target", "className"] D.string )
        ( D.at ["target", "dataset", "id"] D.string |> D.andThen strToIntDecoder )
        ( D.map2 Point
          ( D.field "clientX" D.int )
          ( D.field "clientY" D.int )
        )
    , D.succeed (Mouse Down)
    ]


dragSub : Sub Msg
dragSub =
  Sub.batch
    [ Events.onMouseMove <| D.map Mouse <| D.map Move
        ( D.map2 Point
          ( D.field "clientX" D.int )
          ( D.field "clientY" D.int )
        )
    , Events.onMouseUp <| D.map Mouse <| D.succeed Up
    ]


strToIntDecoder : String -> D.Decoder Int
strToIntDecoder str =
  case String.toInt str of
    Just int -> D.succeed int
    Nothing -> D.fail <| "\"" ++ str ++ "\" is an invalid ID"



-- DEBUG


illegalItemId : String -> Int -> a -> a
illegalItemId func id val =
  illegalId func "Item" id val


illegalId : String -> String -> Int -> a -> a
illegalId func item id val =
  logError func (String.fromInt id ++ " is an illegal " ++ item ++ " ID") val
