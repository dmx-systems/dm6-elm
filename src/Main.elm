module Main exposing (..)

import Model exposing (..)
import Style exposing (..)

import Array
import Browser
import Browser.Events as Events
import Dict exposing (Dict)
import Html exposing (Html, div, text, button, h2)
import Html.Attributes exposing (class, attribute)
import Html.Events exposing (onClick, on)
import String exposing (String, fromInt)
import Svg exposing (Svg, svg, line)
import Svg.Attributes exposing (viewBox, width, height)
import Json.Decode as D
import Debug exposing (log)



-- CONFIG


colors = Array.fromList([120, 0, 210, 36, 270, 58])



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
      []
      NoDrag
      0
  , Cmd.none
  )



-- VIEW


view : Model -> Browser.Document Msg
view model =
  Browser.Document
    "Elm DM6"
    [ div
      ( [ on "mouseover" overDecoder ]
        ++ appStyle
      )
      [ h2 [] [ text "Elm DM6" ]
      , button [ onClick AddTopic ] [ text "Add Topic" ]
      , viewGraph model
      ]
    ]


viewGraph : Model -> Html Msg
viewGraph model =
  let
    ( nodes, edges ) =
      model.items |> Dict.values |> List.foldr
        (\item ( n, e ) ->
            case item of
              Topic topic -> ( viewTopic model topic :: n, e )
              Assoc assoc -> ( n, viewAssoc model assoc :: e )
        )
        ( [], [] )
  in
  div
    []
    [ div
        []
        nodes
    , svg
        ( [ width "800"
          , height "600"
          , viewBox ("0 0 800 600")
          ]
          ++ svgStyle
        )
        edges
    ]


viewTopic : Model -> TopicInfo -> Html Msg
viewTopic model topic =
  div
    ( [ class "dmx-topic"
      , attribute "data-id" (fromInt topic.id)
      ]
      ++ topicStyle model topic
    )
    []


viewAssoc : Model -> AssocInfo -> Svg Msg
viewAssoc model assoc =
  let
    geom = lineGeometry model assoc
  in
  case geom of
    Just ( pos1, pos2 ) ->
      line
        (lineStyle pos1 pos2)
        []
    Nothing -> text "" -- TODO


lineGeometry : Model -> AssocInfo -> Maybe ( Point, Point )
lineGeometry model assoc =
  let
    topic1 = topicPlayer model assoc .player1
    topic2 = topicPlayer model assoc .player2
  in
    Maybe.map2
      (\ t1 t2 -> ( t1.pos, t2.pos ))
      topic1
      topic2


topicPlayer : Model -> AssocInfo -> (AssocInfo -> Id) -> Maybe TopicInfo
topicPlayer model assoc playerFunc =
  let
    playerId = playerFunc assoc
    topic_ = case model.items |> Dict.get playerId of
      Just (Topic topic) -> Just topic
      Just (Assoc _) -> Nothing
      Nothing -> illegalItemId "lineGeometry" playerId Nothing
  in
    topic_



-- UPDATE


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
  let
    _ =
      case msg of
        Mouse (Move _) -> msg
        _ -> log "update" msg
  in
  case msg of
    AddTopic -> ( addTopic model, Cmd.none )
    Mouse mouseMsg -> updateMouse mouseMsg model


addTopic : Model -> Model
addTopic model =
  let
    id = model.nextId
    index = modBy (Array.length colors) (Dict.size model.items)
    color = case colors |> Array.get index of
      Just color_ -> color_
      Nothing -> logError "addTopic" "Illegal color" 0
  in
  { model
  | items = model.items |> Dict.insert id
    ( Topic <| TopicInfo id (Point 112 76) color )
  , nextId = id + 1
  }


addHierarchyAssoc : Model -> Id -> Id -> Model
addHierarchyAssoc model childId parentId =
  let
    id = model.nextId
  in
  { model
  | items = model.items |> Dict.insert id
    ( Assoc <| AssocInfo id childId "dmx.child" parentId "dmx.parent" )
  , nextId = id + 1
  }


updateMouse : MouseMsg -> Model -> ( Model, Cmd Msg )
updateMouse msg model =
  case msg of
    Down -> ( mouseDown model, Cmd.none )
    DownItem class id pos -> ( mouseDownOnItem model class id pos, Cmd.none )
    Move pos -> ( mouseMove model pos, Cmd.none )
    Up -> mouseUp model
    Over class id -> ( mouseOver model class id, Cmd.none )


mouseDown : Model -> Model
mouseDown model =
  { model | selection = [] }


mouseDownOnItem : Model -> Class -> Id -> Point -> Model
mouseDownOnItem model class id pos =
  { model
  | selection = [ id ]
  , dragState = DragEngaged class id pos
  }


mouseMove : Model -> Point -> Model
mouseMove model pos =
  case model.dragState of
    DragEngaged class id pos_ ->
      -- enter DragTopic state only on 1st move
      let
        dragState = case class of
          "dmx-topic" -> DragTopic id pos_ Nothing
          _ -> NoDrag -- the error will be logged in performDrag
      in
      performDrag { model | dragState = dragState } pos
    DragTopic _ _ _ ->
      performDrag model pos
    NoDrag ->
      logError "mouseMove" "Received Move message when dragState is NoDrag" model


performDrag : Model -> Point -> Model
performDrag model pos =
  case model.dragState of
    DragTopic id lastPoint target ->
      let
        delta = Point
          (pos.x - lastPoint.x)
          (pos.y - lastPoint.y)
      in
      { model
        | items = updateTopicPos model id delta
        , dragState = DragTopic id pos target -- update lastPoint
      }
    DragEngaged _ _ _ ->
      logError "performDrag" "Received Move message when dragState is DragEngaged" model
    NoDrag ->
      logError "performDrag" "Received Move message when dragState is NoDrag" model


updateTopicPos : Model -> Id -> Delta -> Items
updateTopicPos model id delta =
  model.items |> Dict.update
    id
    (\item -> case item of
      Just (Topic topic) ->
        let
          pos = Point (topic.pos.x + delta.x) (topic.pos.y + delta.y)
          topic_ = { topic | pos = pos }
        in
          Just (Topic topic_)
      Just assoc -> Just assoc
      Nothing -> illegalItemId "updateTopicPos" id Nothing
    )


mouseUp : Model -> ( Model, Cmd Msg )
mouseUp model =
  let
    newModel = case model.dragState of
      DragTopic id _ (Just targetId) ->
        log ("--> dropped " ++ fromInt id ++ " on " ++ fromInt targetId)
          addHierarchyAssoc model id targetId
      DragTopic _ _ _ ->
        log "--> no drop" model
      DragEngaged _ _ _ ->
        log "--> not dragged" model
      NoDrag -> logError "mouseUp" "Received Up when dragState is NoDrag" model
  in
    ( { newModel | dragState = NoDrag }, Cmd.none )


mouseOver : Model -> Class -> Id -> Model
mouseOver model class targetId =
  case model.dragState of
    NoDrag -> model
    DragTopic id lastPoint _ ->
      let
        target = if id /= targetId then (Just targetId) else Nothing
      in
      { model | dragState = DragTopic id lastPoint target } -- update drop target
    DragEngaged _ _ _ ->
      logError "mouseOver" "Received Over message when dragState is DragEngaged" model



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
  case model.dragState of
    NoDrag -> mouseDownSub
    DragEngaged _ _ _ -> dragSub
    DragTopic _ _ _ -> dragSub


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
        ( D.map2 Point -- TODO: no double code
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



-- HELPER


-- TODO: no double code
overDecoder : D.Decoder Msg
overDecoder =
  D.map Mouse <| D.map2 Over
    ( D.at ["target", "className"] D.string )
    ( D.at ["target", "dataset", "id"] D.string |> D.andThen strToIntDecoder )



-- DEBUG


illegalItemId : String -> Int -> a -> a
illegalItemId func id val =
  illegalId func "Item" id val


illegalId : String -> String -> Int -> a -> a
illegalId func item id val =
  logError func (fromInt id ++ " is an illegal " ++ item ++ " ID") val
