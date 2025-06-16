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
import Debug exposing (log, toString)



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
  ( { items = Dict.empty
    , maps = Dict.singleton 0 Dict.empty
    , activeMap = 0
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
    expanded = case getSelectedId model of
      Just id -> isExpanded id model.activeMap model
      Nothing -> False
    deleteDisabled = List.isEmpty model.selection
  in
  Browser.Document
    "DM6 Elm"
    [ div
      ( [ on "mouseover" (mouseDecoder Over)
        , on "mouseout" (mouseDecoder Out)
        ]
        ++ appStyle
      )
      [ h2 [] [ text "DM6 Elm" ]
      , button
          [ onClick AddTopic ]
          [ text "Add Topic" ]
      , button
          ( [ onClick (Expand <| not expanded)
            , stopPropagationOnMousedown
            , disabled <| expandDisabled model
            ]
            ++ buttonStyle
          )
          [ text (if expanded then "Collapse" else "Expand") ]
      , button
          ( [ onClick Delete
            , stopPropagationOnMousedown
            , disabled deleteDisabled
            ]
            ++ buttonStyle
          )
          [ text "Delete" ]
      , viewMap model.activeMap model
      ]
    ]


expandDisabled : Model -> Bool
expandDisabled model =
  case model.selection of
    [] -> True
    id :: ids -> not (hasMap id model)


viewMap : Id -> Model -> Html Msg
viewMap mapId model =
  let
    ( topics, assocs ) = case getMap mapId model of
      Just map -> viewItems map model
      Nothing -> ( [], [] )
    isTopLevel = mapId == model.activeMap
    size =
      if isTopLevel then
        { w = "1024", h = "600"}
      else
        { w = fromInt whitebox.width, h = fromInt whitebox.height }
  in
    div
      []
      [ div
          []
          topics
      , svg
          ( [ width size.w
            , height size.h
            , viewBox ("0 0 " ++ size.w ++ " " ++ size.h)
            ]
            ++ svgStyle
          )
          ( assocs
            ++ viewLimboAssoc model
          )
      ]


viewItems : Map -> Model -> ( List (Html Msg), List (Svg Msg) )
viewItems map model =
  map |> Dict.toList |> List.foldr
    (\(id, viewItem) ( t, a ) ->
      case viewItem of
        ViewTopic props ->
          case Dict.get id model.items of
            Just (Topic topic) -> ( viewTopic topic props model :: t, a )
            Just (Assoc _) -> topicMismatch "viewItems" id ( t, a )
            Nothing -> illegalItemId "viewItems" id ( t, a )
        ViewAssoc props ->
          case Dict.get id model.items of
            Just (Assoc assoc) -> ( t, viewAssoc assoc model :: a )
            Just (Topic _) -> assocMismatch "viewItems" id ( t, a )
            Nothing -> illegalItemId "viewItems" id ( t, a )
    )
    ( [], [] )


viewTopic : TopicInfo -> TopicProps -> Model -> Html Msg
viewTopic topic props model =
  let
    isContainer = hasMap topic.id model
    itemCount =
      if isContainer then
        case getMap topic.id model of
          Just map -> Dict.size map
          Nothing -> 0
      else
        0
  in
  div
    ( [ class "dmx-topic"
      , attribute "data-id" (fromInt topic.id)
      ]
      ++ topicStyle topic model
      ++
        if isContainer then
          if props.expanded then
            whiteboxStyle topic props
          else
            containerStyle topic props
        else normalStyle topic props
    )
    ( if isContainer then
        if props.expanded then
          [ viewMap topic.id model ]
        else
          [ div
              itemCountStyle
              [ text <| fromInt itemCount ]
          ]
      else
        []
    )


viewAssoc : AssocInfo -> Model -> Svg Msg
viewAssoc assoc model =
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
      case topicPos topicId model.activeMap model of
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
    pos1 = topicPos assoc.player1 model.activeMap model
    pos2 = topicPos assoc.player2 model.activeMap model
  in
  Maybe.map2 (\p1 p2 -> ( p1, p2 )) pos1 pos2


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
    Expand expanded -> ( expand expanded model, Cmd.none )
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
  | items = model.items |> Dict.insert id ( Topic <| TopicInfo id color )
  , maps = addItemToMap (ViewTopic <| TopicProps id pos False) model.activeMap model
  , nextId = id + 1
  }


addAssoc : Model -> Id -> Id -> Model
addAssoc model player1 player2 =
  let
    id = model.nextId
  in
  { model
  | items = model.items |> Dict.insert id
      ( Assoc <| AssocInfo id player1 "dmx.default" player2 "dmx.default" )
  , maps = addItemToMap (ViewAssoc <| AssocProps id) model.activeMap model
  , nextId = id + 1
  }


moveTopicToMap : Model -> Id -> Id -> Id -> Model
moveTopicToMap model topicId fromMapId toMapId =
  let
    viewItem_ = getViewItemById topicId fromMapId model
  in
  case viewItem_ of
    Just viewItem ->
      { model
      | maps = addItemToMap viewItem toMapId
          { model
          | maps = removeItemFromMap model topicId fromMapId
          }
      , selection = [ toMapId ]
      }
    Nothing -> model


addItemToMap : ViewItem -> Id -> Model -> Maps
addItemToMap viewItem mapId model =
  let
    itemId = case viewItem of -- TODO: unify?
      ViewTopic {id} -> id
      ViewAssoc {id} -> id
    -- create map if not exists
    func =
      if hasMap mapId model then
        identity
      else
        Dict.insert mapId Dict.empty
    newModel = { model | maps = func model.maps }
  in
  updateMaps mapId (Dict.insert itemId viewItem) newModel


removeItemFromMap : Model -> Id -> Id -> Maps
removeItemFromMap model itemId mapId =
  updateMaps mapId (Dict.remove itemId) model


getSelectedId : Model -> Maybe Id
getSelectedId model =
  case model.selection of
    [] -> Nothing
    id :: ids -> Just id


updateMaps : Id -> (Map -> Map) -> Model -> Maps
updateMaps mapId callback model =
  model.maps |> Dict.update mapId
    (\map_ ->
      case map_ of
        Just map -> Just (callback map)
        Nothing -> illegalMapId "updateMaps" mapId Nothing
    )


hasMap : Id -> Model -> Bool
hasMap mapId model =
  Dict.member mapId model.maps


topicPos : Id -> Id -> Model -> Maybe Point
topicPos topicId mapId model =
  case getViewItemById topicId mapId model of
    Just (ViewTopic { pos }) -> Just pos
    Just (ViewAssoc _) -> Nothing
    Nothing -> illegalItemId "topicPos" topicId Nothing


updateTopicPos : Id -> Id -> Delta -> Model -> Maps
updateTopicPos topicId mapId delta model =
  updateTopicProps
    topicId
    mapId
    model
    (\props -> { props | pos = Point (props.pos.x + delta.x) (props.pos.y + delta.y) })


expand : Bool -> Model -> Model
expand expanded model =
  let
    maps =
      case getSelectedId model of
        Just topicId -> updateExpand topicId model.activeMap expanded model
        Nothing -> model.maps
  in
  { model | maps = maps }


updateExpand : Id -> Id -> Bool -> Model -> Maps
updateExpand topicId mapId expanded model =
  updateTopicProps
    topicId
    mapId
    model
    (\props -> { props | expanded = expanded })


isExpanded : Id -> Id -> Model -> Bool
isExpanded topicId mapId model =
  case getViewItemById topicId mapId model of
    Just (ViewTopic { expanded }) -> expanded
    Just (ViewAssoc _) -> False
    Nothing -> illegalItemId "view" topicId False


getViewItemById : Id -> Id -> Model -> Maybe ViewItem
getViewItemById itemId mapId model =
  getMap mapId model |> Maybe.andThen (getViewItem itemId)


getMap : Id -> Model -> Maybe Map
getMap mapId model =
  case model.maps |> Dict.get mapId of
    Just map -> Just map
    Nothing -> illegalMapId "getMap" mapId Nothing


getViewItem : Id -> Map -> Maybe ViewItem
getViewItem itemId map =
  case Dict.get itemId map of
    Just viewItem -> Just viewItem
    Nothing -> illegalItemId "getViewItem" itemId Nothing


updateTopicProps : Id -> Id -> Model -> (TopicProps -> TopicProps) -> Maps
updateTopicProps topicId mapId model callback =
  updateMaps
    mapId
    (Dict.update topicId
      (\viewItem ->
        case viewItem of
          Just (ViewTopic props) -> Just
            (ViewTopic (callback props))
          Just (ViewAssoc _) -> illegalItemId "updateTopicProps" topicId Nothing
          Nothing -> illegalItemId "updateTopicProps" topicId Nothing
      )
    )
    model


delete : Model -> Model
delete model =
  let
    maps = updateMaps
      model.activeMap
      (deleteViewItems model.selection)
      model
  in
  { model
  -- TODO: iterate model.selection only once?
  | items = deleteItems model.selection model.items
  , maps = maps
  , selection = []
  }


-- TODO: unify these 2?

deleteItems : List Id -> Items -> Items
deleteItems ids items =
  case ids of
    [] -> items
    id :: moreIds -> deleteItems moreIds (Dict.remove id items)
    -- TODO: delete assocs where the item is a player


deleteViewItems : List Id -> Map -> Map
deleteViewItems ids map =
  case ids of
    [] -> map
    id :: moreIds -> deleteViewItems moreIds (Dict.remove id map)
    -- TODO: delete assocs where the item is a player


-- Mouse

updateMouse : MouseMsg -> Model -> ( Model, Cmd Msg )
updateMouse msg model =
  case msg of
    Down -> ( mouseDown model, Cmd.none )
    DownItem class id pos -> mouseDownOnItem model class id pos
    Move pos -> mouseMove model pos
    Up -> ( mouseUp model, Cmd.none )
    Over class id -> ( mouseOver model class id, Cmd.none )
    Out class id -> ( mouseOut model class id, Cmd.none )
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
    _ -> logError "timeArrived"
      "Received \"Time\" message when dragState is not WaitForTime"
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
    _ -> logError "mouseMove"
      ("Received \"Move\" message when dragState is " ++ toString model.dragState)
      ( model, Cmd.none )


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
            DragTopic -> updateTopicPos id model.activeMap delta model
            DrawAssoc -> model.maps
      in
      { model
        | maps = maps
        , dragState = Drag dragMode id pos target -- update lastPoint
      }
    _ -> logError "performDrag"
      ("Received \"Move\" message when dragState is " ++ toString model.dragState)
      model


mouseUp : Model -> Model
mouseUp model =
  let
    newModel = case model.dragState of
      Drag DragTopic id _ (Just targetId) ->
        log ("--> dropped " ++ fromInt id ++ " on " ++ fromInt targetId)
          moveTopicToMap model id model.activeMap targetId
      Drag DrawAssoc id _ (Just targetId) ->
        log ("--> assoc drawn from " ++ fromInt id ++ " to " ++ fromInt targetId)
          addAssoc model id targetId
      Drag _ _ _ _ ->
        log "--> drag ended w/o target" model
      DragEngaged _ _ _ _ ->
        log "--> drag aborted w/o moving" model
      _ -> logError "mouseUp"
        ("Received \"Up\" message when dragState is " ++ toString model.dragState)
        model
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
    DragEngaged _ _ _ _ ->
      logError "mouseOver" "Received \"Over\" message when dragState is DragEngaged" model
    _ -> model


mouseOut : Model -> Class -> Id -> Model
mouseOut model class targetId =
  case model.dragState of
    Drag dragMode id lastPoint _ ->
      { model | dragState = Drag dragMode id lastPoint Nothing } -- reset drop target
    _ -> model



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
        ( D.map2 Point -- TODO: no code doubling
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


-- TODO: no code doubling
mouseDecoder : (Class -> Id -> MouseMsg) -> D.Decoder Msg
mouseDecoder msg =
  D.map Mouse <| D.map2 msg
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
