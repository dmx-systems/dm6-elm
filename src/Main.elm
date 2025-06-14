module Main exposing (..)

import Model exposing (..)
import Style exposing (..)

import Array
import Browser
import Browser.Events as Events
import Dict exposing (Dict)
import Html exposing (Html, Attribute, div, text, button, h2)
import Html.Attributes exposing (class, attribute, disabled)
import Html.Events exposing (onClick, on, stopPropagationOn)
import String exposing (String, fromInt)
import Svg exposing (Svg, svg, line)
import Svg.Attributes exposing (viewBox, width, height)
import Task
import Time exposing (posixToMillis)
import Json.Decode as D
import Debug exposing (log)



-- CONFIG


colors = Array.fromList([120, 0, 210, 36, 270, 58])
dragThresholdMillis = 300



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
  ( { activeMap = 0
    , maps = Dict.singleton 0 Dict.empty
    , items = Dict.empty
    , selection = []
    , dragState = NoDrag
    , nextId = 1
    }
  , Cmd.none
  )



-- VIEW


view : Model -> Browser.Document Msg
view model =
  let
    deleteDisabled = List.isEmpty model.selection
  in
  Browser.Document
    "DM6 Elm"
    [ div
      ( [ on "mouseover" overDecoder ]
        ++ appStyle
      )
      [ h2 [] [ text "DM6 Elm" ]
      , button
          [ onClick AddTopic ]
          [ text "Add Topic" ]
      , button
          ( [ onClick Delete
            , stopPropagationOnMousedown
            , disabled deleteDisabled
            ]
            ++ buttonStyle
          )
          [ text "Delete" ]
      , viewMap model
      ]
    ]


viewMap : Model -> Html Msg
viewMap model =
  let
    map_ =
      case model.maps |> Dict.get model.activeMap of
        Just map -> Just (viewMapEntries model map)
        Nothing -> illegalMapId "viewMap" model.activeMap Nothing
  in
  case map_ of
    Just ( topics, assocs ) ->
      div
        []
        [ div
            []
            topics
        , svg
            ( [ width "800"
              , height "600"
              , viewBox ("0 0 800 600")
              ]
              ++ svgStyle
            )
            ( assocs
              ++ viewLimboAssoc model
            )
        ]
    Nothing ->
      text <| "Can't render map " ++ fromInt model.activeMap


viewMapEntries : Model -> Map -> ( List (Html Msg), List (Svg Msg) )
viewMapEntries model map =
  map |> Dict.toList |> List.foldr
    (\(id, entry) ( t, a ) ->
      case entry of
        TopicEntry pos ->
          case Dict.get id model.items of
            Just (Topic topic) -> ( viewTopic model topic pos :: t, a )
            Just (Assoc _) -> topicMismatch "viewMapEntries" id ( t, a )
            Nothing -> illegalItemId "viewMapEntries" id ( t, a )
        AssocEntry ->
          case Dict.get id model.items of
            Just (Assoc assoc) -> ( t, viewAssoc model assoc :: a )
            Just (Topic _) -> assocMismatch "viewMapEntries" id ( t, a )
            Nothing -> illegalItemId "viewMapEntries" id ( t, a )
    )
    ( [], [] )


viewTopic : Model -> TopicInfo -> Point -> Html Msg
viewTopic model topic pos =
  div
    ( [ class "dmx-topic"
      , attribute "data-id" (fromInt topic.id)
      ]
      ++ topicStyle model topic pos
    )
    []


viewAssoc : Model -> AssocInfo -> Svg Msg
viewAssoc model assoc =
  let
    geom = assocGeometry model assoc
  in
  case geom of
    Just ( pos1, pos2 ) -> viewLine pos1 pos2
    Nothing -> text "" -- TODO


viewLimboAssoc : Model -> List (Svg Msg)
viewLimboAssoc model =
  case model.dragState of
    Drag DrawAssoc topicId pos _ ->
      case topicPos model topicId of
        Just pos1 -> [ viewLine pos1 pos ]
        Nothing -> []
    _ -> []


viewLine : Point -> Point -> Svg Msg
viewLine pos1 pos2 =
  line
    (lineStyle pos1 pos2)
    []


assocGeometry : Model -> AssocInfo -> Maybe ( Point, Point )
assocGeometry model assoc =
  let
    pos1 = topicPos model assoc.player1
    pos2 = topicPos model assoc.player2
  in
  Maybe.map2 (\p1 p2 -> ( p1, p2 )) pos1 pos2


topicPos : Model -> Id -> Maybe Point
topicPos model id =
  let
    map_ =
      case Dict.get model.activeMap model.maps of
        Just map -> Just map
        Nothing -> illegalMapId "topicPos" model.activeMap Nothing
    getPos map =
      case Dict.get id map of
        Just (TopicEntry pos) -> Just pos
        Just AssocEntry -> Nothing
        Nothing -> illegalItemId "topicPos" id Nothing
  in
  map_ |> Maybe.andThen getPos


stopPropagationOnMousedown : Attribute Msg
stopPropagationOnMousedown =
  stopPropagationOn "mousedown" <| D.succeed (NoOp, True)



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
    Delete -> ( delete model, Cmd.none )
    Mouse mouseMsg -> updateMouse mouseMsg model
    NoOp -> ( model, Cmd.none )


addTopic : Model -> Model
addTopic model =
  let
    id = model.nextId
    index = modBy (Array.length colors) (Dict.size model.items)
    color = case colors |> Array.get index of
      Just color_ -> color_
      Nothing -> logError "addTopic" "Illegal color" 0
    pos = Point 112 76
  in
  { model
  | maps = model.maps |> Dict.update model.activeMap
      (\map_ ->
        case map_ of
          Just map -> Just (map |> Dict.insert id (TopicEntry pos))
          Nothing -> illegalMapId "addTopic" model.activeMap Nothing
      )
  , items = model.items |> Dict.insert id ( Topic <| TopicInfo id color )
  , nextId = id + 1
  }


addAssoc : Model -> Id -> Id -> Model
addAssoc model player1 player2 =
  let
    id = model.nextId
  in
  { model
  | maps = model.maps |> Dict.update model.activeMap
      (\map_ ->
        case map_ of
          Just map -> Just (map |> Dict.insert id AssocEntry)
          Nothing -> illegalMapId "addAssoc" model.activeMap Nothing
      )
  , items = model.items |> Dict.insert id
      ( Assoc <| AssocInfo id player1 "dmx.default" player2 "dmx.default" )
  , nextId = id + 1
  }


delete : Model -> Model
delete model =
  { model
  | items = deleteItems model.items model.selection
  , selection = []
  }


deleteItems : Items -> List Id -> Items
deleteItems items ids =
  case ids of
    [] -> items
    id :: moreIds -> deleteItems (Dict.remove id items) moreIds
    -- TODO: delete assocs where this item is a player


updateMouse : MouseMsg -> Model -> ( Model, Cmd Msg )
updateMouse msg model =
  case msg of
    Down -> ( mouseDown model, Cmd.none )
    DownItem class id pos -> mouseDownOnItem model class id pos
    Move pos -> mouseMove model pos
    Up -> ( mouseUp model, Cmd.none )
    Over class id -> ( mouseOver model class id, Cmd.none )
    Time time -> ( timeArrived model time, Cmd.none )


mouseDown : Model -> Model
mouseDown model =
  { model | selection = [] }


mouseDownOnItem : Model -> Class -> Id -> Point -> ( Model, Cmd Msg )
mouseDownOnItem model class id pos =
  ( { model
    | selection = [ id ]
    , dragState = WaitForStartTime class id pos
    }
  , Task.perform (Time >> Mouse) Time.now
  )


timeArrived : Model -> Time.Posix -> Model
timeArrived model time =
  case model.dragState of
    WaitForStartTime class id pos ->
      { model | dragState = DragEngaged time class id pos }
    WaitForEndTime startTime class id pos ->
      { model | dragState =
          case class of
            "dmx-topic" ->
              let
                dragMode =
                  if posixToMillis time - posixToMillis startTime < dragThresholdMillis then
                    DragTopic
                  else
                    DrawAssoc
              in
              Drag dragMode id pos Nothing
            _ -> NoDrag -- the error will be logged in performDrag
      }
    _ -> logError "timeArrived" "Received Time message when dragState is not WaitForTime"
        model


mouseMove : Model -> Point -> ( Model, Cmd Msg )
mouseMove model pos =
  case model.dragState of
    DragEngaged time class id pos_ ->
      ( { model | dragState = WaitForEndTime time class id pos_ }
      , Task.perform (Time >> Mouse) Time.now
      )
    WaitForEndTime _ class id pos_ ->
      ( model, Cmd.none ) -- ignore
    Drag _ _ _ _ ->
      ( performDrag model pos, Cmd.none )
    WaitForStartTime _ _ _ ->
      logError "mouseMove" "Received Move message when dragState is WaitForStartTime"
        ( model, Cmd.none )
    NoDrag ->
      logError "mouseMove" "Received Move message when dragState is NoDrag" ( model, Cmd.none )


performDrag : Model -> Point -> Model
performDrag model pos =
  case model.dragState of
    Drag dragMode id lastPoint target ->
      let
        delta = Point
          (pos.x - lastPoint.x)
          (pos.y - lastPoint.y)
        maps =
          case dragMode of
            DragTopic -> updateTopicPos model id delta
            DrawAssoc -> model.maps
      in
      { model
        | maps = maps
        , dragState = Drag dragMode id pos target -- update lastPoint
      }
    WaitForStartTime _ _ _ ->
      logError "performDrag" "Received Move message when dragState is WaitForStartTime" model
    WaitForEndTime _ _ _ _ ->
      logError "performDrag" "Received Move message when dragState is WaitForEndTime" model
    DragEngaged _ _ _ _ ->
      logError "performDrag" "Received Move message when dragState is DragEngaged" model
    NoDrag ->
      logError "performDrag" "Received Move message when dragState is NoDrag" model


updateTopicPos : Model -> Id -> Delta -> Maps
updateTopicPos model id delta =
  model.maps |> Dict.update model.activeMap
    (\map_ ->
      case map_ of
        Just map -> Just
          (map |> Dict.update id
            (\entry_ ->
              case entry_ of
                Just (TopicEntry pos) -> Just
                  (TopicEntry <| Point (pos.x + delta.x) (pos.y + delta.y))
                Just AssocEntry -> illegalItemId "updateTopicPos" id Nothing
                Nothing -> illegalItemId "updateTopicPos" id Nothing
            )
          )
        Nothing -> illegalMapId "addTopic" model.activeMap Nothing
    )


mouseUp : Model -> Model
mouseUp model =
  let
    newModel = case model.dragState of
      Drag DragTopic id _ (Just targetId) ->
        log ("--> dropped " ++ fromInt id ++ " on " ++ fromInt targetId) model
          -- TODO
      Drag DrawAssoc id _ (Just targetId) ->
        log ("--> assoc drawn from " ++ fromInt id ++ " to " ++ fromInt targetId)
          addAssoc model id targetId
      Drag _ _ _ _ ->
        log "--> drag ended w/o target" model
      DragEngaged _ _ _ _ ->
        log "--> drag aborted w/o moving" model
      WaitForStartTime _ _ _ ->
        logError "mouseUp" "Received Up when dragState is WaitForStartTime" model
      WaitForEndTime _ _ _ _ ->
        logError "mouseUp" "Received Up when dragState is WaitForEndTime" model
      NoDrag -> logError "mouseUp" "Received Up when dragState is NoDrag" model
  in
  { newModel | dragState = NoDrag }


mouseOver : Model -> Class -> Id -> Model
mouseOver model class targetId =
  case model.dragState of
    Drag dragMode id lastPoint _ ->
      let
        target = if id /= targetId then (Just targetId) else Nothing
      in
      { model | dragState = Drag dragMode id lastPoint target } -- update drop target
    NoDrag -> model
    WaitForStartTime _ _ _ -> model -- TODO: Error?
    WaitForEndTime _ _ _ _ -> model -- TODO: Error?
    DragEngaged _ _ _ _ ->
      logError "mouseOver" "Received Over message when dragState is DragEngaged" model



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
  case model.dragState of
    NoDrag -> mouseDownSub
    WaitForStartTime _ _ _ -> Sub.none
    WaitForEndTime _ _ _ _ -> Sub.none
    DragEngaged _ _ _ _ -> dragSub
    Drag _ _ _ _ -> dragSub


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


topicMismatch : String -> Int -> a -> a
topicMismatch func id val =
  logError func (fromInt id ++ " is not a Topic but an Assoc") val


assocMismatch : String -> Int -> a -> a
assocMismatch func id val =
  logError func (fromInt id ++ " is not an Assoc but a Topic") val


illegalMapId : String -> Int -> a -> a
illegalMapId func id val =
  illegalId func "Map" id val


illegalItemId : String -> Int -> a -> a
illegalItemId func id val =
  illegalId func "Item" id val


illegalId : String -> String -> Int -> a -> a
illegalId func item id val =
  logError func (fromInt id ++ " is an illegal " ++ item ++ " ID") val
