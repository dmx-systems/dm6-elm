module Main exposing (..)

import Model exposing (..)
import Style exposing (..)

import Array
import Browser
import Browser.Events as Events
import Dict exposing (Dict)
import Html exposing (Html, Attribute, div, text, button, input, label, h1)
import Html.Attributes exposing (class, attribute, type_, name, checked, disabled, style)
import Html.Events exposing (onClick, on, stopPropagationOn)
import Random
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
    , maps = Dict.singleton 0 <| Map 0 -1 Dict.empty -- parent = -1
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
      [ h1 [] [ text "DM6 Elm" ]
      , div
          toolbarStyle
          [ button
              ( [ onClick CreateTopic ]
                ++ buttonStyle
              )
              [ text "Add Topic" ]
          , viewDisplayMode model
          , button
              ( [ onClick Delete
                , stopPropagationOnMousedown
                , disabled deleteDisabled
                ]
                ++ buttonStyle
              )
              [ text "Delete" ]
          ]
      , viewMap model.activeMap -1 model -- top-level map has parentMapId -1
      ]
    ]


viewDisplayMode : Model -> Html Msg
viewDisplayMode model =
  let
    displayMode = case getSingleSelection model of
      Just (topicId, mapId) -> getDisplayMode topicId mapId model
      Nothing -> Nothing
    checked1 = displayMode == Just BlackBox
    checked2 = displayMode == Just WhiteBox
    checked3 = displayMode == Just Unboxed
    disabled_ = displayMode == Nothing
  in
  div
    (displayModeStyle disabled_)
    [ div
        []
        [ text "Children Display" ]
    , label
        [ onClick (Set <| Just BlackBox), stopPropagationOnMousedown ]
        [ input
            [ type_ "radio", name "display-mode", checked checked1, disabled disabled_ ]
            []
        , text "Black Box"
        ]
    , label
        [ onClick (Set <| Just WhiteBox), stopPropagationOnMousedown ]
        [ input
            [ type_ "radio", name "display-mode", checked checked2, disabled disabled_ ]
            []
        , text "White Box"
        ]
    , label
        [ onClick (Set <| Just Unboxed), stopPropagationOnMousedown ]
        [ input
            [ type_ "radio", name "display-mode", checked checked3, disabled disabled_ ]
            []
        , text "Unboxed"
        ]
    ]


viewMap : MapId -> MapId -> Model -> Html Msg
viewMap mapId parentMapId model =
  let
    (topics, assocs) = case getMap mapId model of
      Just map -> viewItems map model
      Nothing -> ( [], [] )
    isTopLevel = mapId == model.activeMap
    size =
      if isTopLevel then
        { w = "1024", h = "600"}
      else
        { w = fromInt whitebox.width, h = fromInt whitebox.height }
    -- For nested maps give the outer div both the topic meta data and a size. So mouse events
    -- can land there and are detected as mousedown-on-item. Otherwise the target would be the
    -- SVG but our event decoder does not work there.
    nestedMapAttributes =
      if isTopLevel then
        []
      else
        topicAttr mapId parentMapId
        ++
        [ style "position" "absolute"
        , style "width" "100%"
        , style "height" "100%"
        ]
  in
    div
      nestedMapAttributes
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
            ++ viewLimboAssoc mapId model
          )
      ]


viewItems : Map -> Model -> ( List (Html Msg), List (Svg Msg) )
viewItems map model =
  map.items |> Dict.toList |> List.foldr
    (\(id, viewItem) ( t, a ) ->
      case viewItem.viewProps of
        ViewTopic props ->
          case Dict.get id model.items of
            Just (Topic topic) -> ( viewTopic topic props map.id model :: t, a )
            Just (Assoc _) -> topicMismatch "viewItems" id ( t, a )
            Nothing -> illegalItemId "viewItems" id ( t, a )
        ViewAssoc props ->
          case Dict.get id model.items of
            Just (Assoc assoc) -> ( t, viewAssoc assoc map.id model :: a )
            Just (Topic _) -> assocMismatch "viewItems" id ( t, a )
            Nothing -> illegalItemId "viewItems" id ( t, a )
    )
    ( [], [] )


viewTopic : TopicInfo -> TopicProps -> MapId -> Model -> Html Msg
viewTopic topic props mapId model =
  div
    ( topicAttr topic.id mapId
      ++ topicStyle topic mapId model
      ++
        case props.displayMode of
          Just BlackBox -> blackBoxStyle topic props
          Just WhiteBox -> whiteboxStyle topic props
          Just Unboxed -> normalStyle topic props
          Nothing -> normalStyle topic props
    )
    ( case props.displayMode of
        Just BlackBox -> viewItemCount topic.id props model
        Just WhiteBox -> [ viewMap topic.id mapId model ]
        Just Unboxed -> []
        Nothing -> []
    )


viewItemCount : Id -> TopicProps -> Model -> List (Html Msg)
viewItemCount topicId props model =
  let
    itemCount =
      if props.displayMode /= Nothing then
        case getMap topicId model of
          Just map -> map.items |> Dict.size
          Nothing -> 0
      else
        0
  in
    [ div
        itemCountStyle
        [ text <| fromInt itemCount ]
    ]


topicAttr : Id -> MapId -> List (Attribute Msg)
topicAttr id mapId =
  [ class "dmx-topic"
  , attribute "data-id" (fromInt id)
  , attribute "data-map-id" (fromInt mapId)
  ]


viewAssoc : AssocInfo -> MapId -> Model -> Svg Msg
viewAssoc assoc mapId model =
  let
    geom = assocGeometry assoc mapId model
  in
  case geom of
    Just ( pos1, pos2 ) -> viewLine pos1 pos2
    Nothing -> text "" -- TODO


viewLimboAssoc : MapId -> Model -> List (Svg Msg)
viewLimboAssoc mapId model =
  case model.dragState of
    Drag DrawAssoc topicId mapId_ pos _ ->
      if mapId_ == mapId then
        let
          points = Maybe.map2
            (\pos1 pos2 -> (pos1, pos2))
            (topicPos topicId mapId model)
            (relPos pos mapId model)
        in
        case points of
          Just (pos1, pos2) -> [ viewLine pos1 pos2 ]
          Nothing -> []
      else
        []
    _ -> []


relPos : Point -> MapId -> Model -> Maybe Point
relPos pos mapId model =
  offsetPos mapId (Point 0 0) model |> Maybe.andThen
    (\offset -> Just (Point (pos.x - offset.x) (pos.y - offset.y)))


offsetPos : MapId -> Point -> Model -> Maybe Point
offsetPos mapId offset model =
  if mapId == model.activeMap then
    Just offset
  else
    let
      nextMap =
        getParentMapId mapId model
      nextOffset =
        mapPos mapId model |> Maybe.andThen
          (\pos ->
            Just
              ( Point
                  (offset.x + pos.x - whitebox.width // 2)
                  (offset.y + pos.y - whitebox.height // 2)
              )
          )
      pos_ =
        Maybe.map2
          (\parentMapId offset_ ->
            offsetPos parentMapId offset_ model
          )
          nextMap
          nextOffset
    in
      case pos_ of
        Just p -> p
        Nothing -> Nothing


mapPos : MapId -> Model -> Maybe Point
mapPos mapId model =
  getParentMapId mapId model |> Maybe.andThen
    (\parentMapId -> topicPos mapId parentMapId model)


getParentMapId : MapId -> Model -> Maybe MapId
getParentMapId mapId model =
  getMap mapId model |> Maybe.andThen (\map -> Just map.parent)


viewLine : Point -> Point -> Svg Msg
viewLine pos1 pos2 =
  line
    (lineStyle pos1 pos2)
    []


assocGeometry : AssocInfo -> MapId -> Model -> Maybe ( Point, Point )
assocGeometry assoc mapId model =
  let
    pos1 = topicPos assoc.player1 mapId model
    pos2 = topicPos assoc.player2 mapId model
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
        Mouse _ -> msg
        _ -> info "update" msg
  in
  case msg of
    CreateTopic -> ( createTopicAndAddToMap model, Cmd.none )
    MoveTopicToMap topicId fromMapId targetId targetMapId pos
      -> ( moveTopicToMap topicId fromMapId targetId targetMapId pos model, Cmd.none )
    Set displayMode -> ( setDisplayMode displayMode model, Cmd.none )
    Delete -> ( delete model, Cmd.none )
    Mouse mouseMsg -> updateMouse mouseMsg model
    NoOp -> ( model, Cmd.none )


createTopic : Model -> (Model, Id)
createTopic model =
  let
    id = model.nextId
    index = modBy (Array.length colors) (topicCount model)
    color = case colors |> Array.get index of
      Just color_ -> color_
      Nothing -> logError "createTopic" "Illegal color" 0
  in
  ( { model
    | items = model.items |> Dict.insert id ( Topic <| TopicInfo id color )
    , nextId = id + 1
    }
  , id
  )


createTopicAndAddToMap : Model -> Model
createTopicAndAddToMap model =
  let
    (newModel, topicId) = createTopic model
    pos = Point 112 86
  in
  addItemToMap topicId (ViewTopic <| TopicProps pos Nothing) model.activeMap newModel


-- Presumption: both players exist in same map
createDefaultAssoc : Id -> Id -> MapId -> Model -> Model
createDefaultAssoc player1 player2 mapId model =
  createAssocAndAddToMap
    "dmx.association"
    player1 "dmx.default"
    player2 "dmx.default"
    mapId model


-- Presumption: both players exist in same map
createAssoc : ItemType -> Id -> RoleType -> Id -> RoleType -> Model -> (Model, Id)
createAssoc itemType player1 role1 player2 role2 model =
  let
    id = model.nextId
  in
  ( { model
    | items = model.items |> Dict.insert id
        ( Assoc <| AssocInfo id itemType player1 role1 player2 role2 )
    , nextId = id + 1
    }
  , id
  )


-- Presumption: both players exist in same map
createAssocAndAddToMap : ItemType -> Id -> RoleType -> Id -> RoleType -> MapId -> Model -> Model
createAssocAndAddToMap itemType player1 role1 player2 role2 mapId model =
  let
    (newModel, assocId) = createAssoc itemType player1 role1 player2 role2 model
  in
  addItemToMap assocId (ViewAssoc AssocProps) mapId newModel


moveTopicToMap : Id -> MapId -> Id -> MapId -> Point -> Model -> Model
moveTopicToMap topicId fromMapId targetId targetMapId pos model =
  let
    viewProps_ = getTopicProps topicId fromMapId model |> Maybe.andThen
      (\props -> Just (ViewTopic { props | pos = pos }))
    -- create map if not exists
    newModel =
      if hasMap targetId model then
        model
      else
        { model
        | maps = updateDisplayMode targetId targetMapId (Just BlackBox)
            { model
            | maps = model.maps |> Dict.insert targetId (Map targetId targetMapId Dict.empty)
            }
        }
  in
  case viewProps_ of
    Just viewProps ->
      addItemToMap topicId viewProps targetId
        { newModel
        | maps = removeItemFromMap topicId fromMapId newModel
        , selection = [ (targetId, targetMapId) ]
        }
    Nothing -> model


addItemToMap : Id -> ViewProps -> MapId -> Model -> Model
addItemToMap itemId props mapId model =
  let
    (newModel, assocId) = createAssoc
      "dmx.composition"
      itemId "dmx.child"
      mapId "dmx.parent"
      model
    viewItem = ViewItem itemId props assocId
  in
  { newModel | maps =
    updateMaps
      mapId
      (\map -> { map | items = map.items |> Dict.insert itemId viewItem })
      newModel
  }


getSingleSelection : Model -> Maybe (Id, MapId)
getSingleSelection model =
  case model.selection of
    [] -> Nothing
    selItem :: selItems -> Just selItem
    -- FIXME: return nothing if more than one item


topicPos : Id -> MapId -> Model -> Maybe Point
topicPos topicId mapId model =
  case getTopicProps topicId mapId model of
    Just { pos } -> Just pos
    Nothing -> fail "topicPos" {topicId = topicId, mapId = mapId} Nothing


updateTopicPos : Id -> Id -> Delta -> Model -> Maps
updateTopicPos topicId mapId delta model =
  updateTopicProps
    topicId
    mapId
    model
    (\props -> { props | pos = Point (props.pos.x + delta.x) (props.pos.y + delta.y) })


setDisplayMode : Maybe DisplayMode -> Model -> Model
setDisplayMode displayMode model =
  let
    maps =
      case getSingleSelection model of
        Just (topicId, mapId) ->
          let
            newModel =
              { model | maps =
                  case displayMode of
                    Just BlackBox -> boxContainer topicId mapId model
                    Just WhiteBox -> boxContainer topicId mapId model
                    Just Unboxed -> unboxContainer topicId mapId model
                    Nothing -> model.maps
              }
          in
          updateDisplayMode topicId mapId displayMode newModel
        Nothing -> model.maps
  in
  { model | maps = maps }


updateDisplayMode : Id -> MapId -> Maybe DisplayMode -> Model -> Maps
updateDisplayMode topicId mapId displayMode model =
  updateTopicProps
    topicId
    mapId
    model
    (\props -> { props | displayMode = displayMode })


getDisplayMode : Id -> MapId -> Model -> Maybe DisplayMode
getDisplayMode topicId mapId model =
  case getTopicProps topicId mapId model of
    Just { displayMode } -> displayMode
    Nothing -> fail "getDisplayMode" {topicId = topicId, mapId = mapId} Nothing


-- TODO: consolidate these 2 functions

boxContainer : Id -> MapId -> Model -> Maps
boxContainer topicId mapId model =
  let
    maps_ = getMap topicId model |> Maybe.andThen
      (\fromMap -> Just
        (updateMaps
          mapId
          (\toMap ->
            { toMap | items = boxItems fromMap.items toMap.items model }
          )
          model
        )
      )
  in
  case maps_ of
    Just maps -> maps
    Nothing -> model.maps


unboxContainer : Id -> MapId -> Model -> Maps
unboxContainer containerId targetMapId model =
  let
    maps_ = getMap containerId model |> Maybe.andThen
      (\containerMap -> Just
        (updateMaps
          targetMapId
          (\targetMap ->
            { targetMap | items =
              Dict.union targetMap.items (unboxItems containerMap.items targetMapId model)
            }
          )
          model
        )
      )
  in
  case maps_ of
    Just maps -> maps
    Nothing -> model.maps


boxItems : ViewItems -> ViewItems -> Model -> ViewItems
boxItems fromItems toItems model =
  fromItems |> Dict.values |> List.foldr
    (\viewItem newItems ->
      let
        assocId = viewItem.mapAssocId
        items = Dict.remove viewItem.id newItems |> Dict.remove assocId
      in
      case getMapIfExists viewItem.id model of
        Just map -> boxItems map.items items model
        Nothing -> items
    )
    toItems


unboxItems : ViewItems -> MapId -> Model -> ViewItems
unboxItems sourceItems targetMapId model =
  sourceItems |> Dict.values |> List.foldr
    (\viewItem newItems ->
      let
        newItem = targetViewItem viewItem
        assocId = viewItem.mapAssocId
        assocItem = ViewItem assocId (ViewAssoc AssocProps) -1
        items = Dict.insert viewItem.id newItem newItems |> Dict.insert assocId assocItem
        deepItems = case getMapIfExists viewItem.id model of
          Just map -> unboxItems map.items targetMapId model
          Nothing -> Dict.empty
      in
      Dict.union items deepItems
    )
    Dict.empty


targetViewItem : ViewItem -> ViewItem
targetViewItem viewItem =
  { viewItem | viewProps =
    case viewItem.viewProps of
      ViewTopic props -> ViewTopic { props | displayMode = Just Unboxed }
      ViewAssoc props -> ViewAssoc props
  }


getTopicProps : Id -> MapId -> Model -> Maybe TopicProps
getTopicProps topicId mapId model =
  case getViewItemById topicId mapId model of
    Just viewItem ->
      case viewItem.viewProps of
        ViewTopic props -> Just props
        ViewAssoc _ -> topicMismatch "getTopicProps" topicId Nothing
    Nothing -> fail "getTopicProps" {topicId = topicId, mapId = mapId} Nothing


getViewItemById : Id -> MapId -> Model -> Maybe ViewItem
getViewItemById itemId mapId model =
  getMap mapId model |> Maybe.andThen (getViewItem itemId)


getViewItem : Id -> Map -> Maybe ViewItem
getViewItem itemId map =
  case map.items |> Dict.get itemId of
    Just viewItem -> Just viewItem
    Nothing -> itemNotInMap "getViewItem" itemId map.id Nothing


updateTopicProps : Id -> Id -> Model -> (TopicProps -> TopicProps) -> Maps
updateTopicProps topicId mapId model propsFunc =
  updateMaps
    mapId
    (updateTopicProps_ topicId propsFunc)
    model


delete : Model -> Model
delete model =
  let
    maps = updateMaps
      model.activeMap -- FIXME: delete items from other/different maps
      (deleteViewItems model.selection)
      model
  in
  { model
  -- TODO: iterate model.selection only once?
  | items = deleteItems model.selection model.items
  , maps = maps
  , selection = []
  }


deleteItems : Selection -> Items -> Items
deleteItems selItems items =
  case selItems of
    [] -> items
    (id, mapId) :: moreSelItems -> deleteItems moreSelItems (Dict.remove id items)
    -- FIXME: delete assocs where the item is a player


deleteViewItems : Selection -> Map -> Map
deleteViewItems selItems map =
  case selItems of
    [] -> map
    (id, mapId) :: moreSelItems -> deleteViewItems moreSelItems (removeItemFromMap_ id map)
    -- FIXME: delete assocs where the item is a player


removeItemFromMap : Id -> Id -> Model -> Maps
removeItemFromMap itemId mapId model =
  updateMaps mapId (removeItemFromMap_ itemId) model


removeItemFromMap_ : Id -> Map -> Map
removeItemFromMap_ itemId map =
  { map | items = map.items |> Dict.remove itemId }


updateTopicProps_ : Id -> (TopicProps -> TopicProps) -> Map -> Map
updateTopicProps_ topicId propsFunc map =
  { map | items = map.items |> Dict.update topicId
    (\viewItem_ ->
      case viewItem_ of
        Just viewItem ->
          case viewItem.viewProps of
            ViewTopic props -> Just
              { viewItem | viewProps = ViewTopic (propsFunc props) }
            ViewAssoc _ -> illegalItemId "updateTopicProps" topicId Nothing
        Nothing -> illegalItemId "updateTopicProps" topicId Nothing
    )
  }


updateMaps : MapId -> (Map -> Map) -> Model -> Maps
updateMaps mapId mapFunc model =
  model.maps |> Dict.update mapId
    (\map_ ->
      case map_ of
        Just map -> Just (mapFunc map)
        Nothing -> illegalMapId "updateMaps" mapId Nothing
    )


getMap : MapId -> Model -> Maybe Map
getMap mapId model =
  case getMapIfExists mapId model of
    Just map -> Just map
    Nothing -> illegalMapId "getMap" mapId Nothing


getMapIfExists : MapId -> Model -> Maybe Map
getMapIfExists mapId model =
  case model.maps |> Dict.get mapId of
    Just map -> Just map
    Nothing -> Nothing


hasMap : MapId -> Model -> Bool
hasMap mapId model =
  model.maps |> Dict.member mapId


-- Mouse

updateMouse : MouseMsg -> Model -> ( Model, Cmd Msg )
updateMouse msg model =
  case msg of
    Down -> ( mouseDown model, Cmd.none )
    DownItem class id mapId pos -> mouseDownOnItem model class id mapId pos
    Move pos -> mouseMove model pos
    Up -> mouseUp model
    Over class id mapId -> ( mouseOver model class id mapId, Cmd.none )
    Out class id mapId -> ( mouseOut model class id mapId, Cmd.none )
    Time time -> ( timeArrived model time, Cmd.none )


mouseDown : Model -> Model
mouseDown model =
  { model | selection = [] }


mouseDownOnItem : Model -> Class -> Id -> MapId -> Point -> ( Model, Cmd Msg )
mouseDownOnItem model class id mapId pos =
  ( { model
    | selection = [ (id, mapId) ]
    , dragState = WaitForStartTime class id mapId pos
    }
  , Task.perform (Time >> Mouse) Time.now
  )


timeArrived : Model -> Time.Posix -> Model
timeArrived model time =
  case model.dragState of
    WaitForStartTime class id mapId pos ->
      { model | dragState = DragEngaged time class id mapId pos }
    WaitForEndTime startTime class id mapId pos ->
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
              Drag dragMode id mapId pos Nothing
            _ -> NoDrag -- the error will be logged in performDrag
      }
    _ -> logError "timeArrived"
      "Received \"Time\" message when dragState is not WaitForTime"
      model


mouseMove : Model -> Point -> ( Model, Cmd Msg )
mouseMove model pos =
  case model.dragState of
    DragEngaged time class id mapId pos_ ->
      ( { model | dragState = WaitForEndTime time class id mapId pos_ }
      , Task.perform (Time >> Mouse) Time.now
      )
    WaitForEndTime _ _ _ _ _ ->
      ( model, Cmd.none ) -- ignore -- TODO: can this happen at all? Is there a move listener?
    Drag _ _ _ _ _ ->
      ( performDrag model pos, Cmd.none )
    _ -> logError "mouseMove"
      ("Received \"Move\" message when dragState is " ++ toString model.dragState)
      ( model, Cmd.none )


performDrag : Model -> Point -> Model
performDrag model pos =
  case model.dragState of
    Drag dragMode id mapId lastPoint target ->
      let
        delta = Point
          (pos.x - lastPoint.x)
          (pos.y - lastPoint.y)
        maps =
          case dragMode of
            DragTopic -> updateTopicPos id mapId delta model
            DrawAssoc -> model.maps
      in
      { model
        | maps = maps
        , dragState = Drag dragMode id mapId pos target -- update lastPoint
      }
    _ -> logError "performDrag"
      ("Received \"Move\" message when dragState is " ++ toString model.dragState)
      model


mouseUp : Model -> ( Model, Cmd Msg )
mouseUp model =
  let
    ( newModel, cmd ) = case model.dragState of
      Drag DragTopic id mapId _ (Just (targetId, targetMapId)) ->
        let
          _ = info "mouseUp" ("dropped " ++ fromInt id ++ " (map " ++ fromInt mapId ++
            ") on " ++ fromInt targetId ++ " (map " ++ fromInt targetMapId ++ ")")
        in
        ( model, Random.generate (MoveTopicToMap id mapId targetId targetMapId) point )
      Drag DrawAssoc id mapId _ (Just (targetId, _)) -> -- target map ID not used here
        let
          _ = info "mouseUp" ("assoc drawn from " ++ fromInt id ++ " (map " ++
            fromInt mapId ++ ") to " ++ fromInt targetId)
        in
        ( createDefaultAssoc id targetId mapId model, Cmd.none)
      Drag _ _ _ _ _ ->
        let
          _ = info "mouseUp" "drag ended w/o target"
        in
        ( model, Cmd.none )
      DragEngaged _ _ _ _ _ ->
        let
          _ = info "mouseUp" "drag aborted w/o moving"
        in
        ( model, Cmd.none )
      _ ->
        logError "mouseUp"
          ("Received \"Up\" message when dragState is " ++ toString model.dragState)
          ( model, Cmd.none )
  in
  ( { newModel | dragState = NoDrag }, cmd )


point : Random.Generator Point
point =
  Random.map2 Point
    (Random.int 0 whitebox.width)
    (Random.int 0 whitebox.height)


mouseOver : Model -> Class -> Id -> MapId -> Model
mouseOver model class targetId targetMapId =
  case model.dragState of
    Drag dragMode id mapId lastPoint _ ->
      let
        target =
          if (id, mapId) /= (targetId, targetMapId) then
            (Just (targetId, targetMapId))
          else
            Nothing
      in
      { model | dragState = Drag dragMode id mapId lastPoint target } -- update drop target
    DragEngaged _ _ _ _ _ ->
      logError "mouseOver" "Received \"Over\" message when dragState is DragEngaged" model
    _ -> model


mouseOut : Model -> Class -> Id -> MapId -> Model
mouseOut model class targetId targetMapId =
  case model.dragState of
    Drag dragMode id mapId lastPoint _ ->
      { model | dragState = Drag dragMode id mapId lastPoint Nothing } -- reset drop target
    _ -> model



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
  case model.dragState of
    NoDrag -> mouseDownSub
    WaitForStartTime _ _ _ _ -> Sub.none
    WaitForEndTime _ _ _ _ _ -> Sub.none
    DragEngaged _ _ _ _ _ -> dragSub
    Drag _ _ _ _ _ -> dragSub


mouseDownSub : Sub Msg
mouseDownSub =
  Events.onMouseDown <| D.oneOf
    [ D.map Mouse <| D.map4 DownItem
        ( D.at ["target", "className"] D.string )
        ( D.at ["target", "dataset", "id"] D.string |> D.andThen strToIntDecoder )
        ( D.at ["target", "dataset", "mapId"] D.string |> D.andThen strToIntDecoder )
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
mouseDecoder : (Class -> Id -> MapId -> MouseMsg) -> D.Decoder Msg
mouseDecoder msg =
  D.map Mouse <| D.map3 msg
    ( D.at ["target", "className"] D.string )
    ( D.at ["target", "dataset", "id"] D.string |> D.andThen strToIntDecoder )
    ( D.at ["target", "dataset", "mapId"] D.string |> D.andThen strToIntDecoder )


topicCount : Model -> Int
topicCount model =
  model.items |> Dict.values |> List.filter topicFilter |>  List.length


topicFilter : Item -> Bool
topicFilter item =
  case item of
    Topic _ -> True
    Assoc _ -> False



-- DEBUG


itemNotInMap : String -> Id -> Id -> a -> a
itemNotInMap funcName itemId mapId val =
  logError funcName ("item " ++ fromInt itemId ++ " not in map " ++ fromInt mapId) val


topicMismatch : String -> Id -> a -> a
topicMismatch funcName id val =
  logError funcName (fromInt id ++ " is not a Topic but an Assoc") val


assocMismatch : String -> Id -> a -> a
assocMismatch funcName id val =
  logError funcName (fromInt id ++ " is not an Assoc but a Topic") val


illegalMapId : String -> Id -> a -> a
illegalMapId funcName id val =
  illegalId funcName "Map" id val


illegalItemId : String -> Id -> a -> a
illegalItemId funcName id val =
  illegalId funcName "Item" id val


illegalId : String -> String -> Id -> a -> a
illegalId funcName item id val =
  logError funcName (fromInt id ++ " is an illegal " ++ item ++ " ID") val

--

logError : String -> String -> v -> v
logError funcName text val =
  log ("### ERROR @" ++ funcName ++ ": " ++ text) val


fail : String -> a -> v -> v
fail funcName args val =
  log ("--> @" ++ funcName ++ " failed " ++ toString args) val


call : String -> a -> v -> v
call funcName args val =
  log ("@" ++ funcName ++ " " ++ toString args ++ " -->") val


info : String -> v -> v
info funcName val =
  log ("@" ++ funcName) val
