module Main exposing (..)

import Model exposing (..)
import Style exposing (..)

import Array
import Browser
import Browser.Events as Events
import Dict exposing (Dict)
import Html exposing (Html, Attribute, div, text, button, input, label, h1)
import Html.Attributes exposing (class, attribute, type_, name, checked, disabled)
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
dragThresholdMillis = 200



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
    , maps = Dict.singleton 0
      <| Map 0 Dict.empty (Rectangle 0 0 0 0) (Offset 0 0) -1 -- parentMapId = -1
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
      Just (topicId, mapId) -> getDisplayMode topicId mapId model.maps
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
        [ text "Content Display" ]
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
    isTopLevel = mapId == model.activeMap
    ((topics, assocs), rect, vb) =
      case getMap mapId model.maps of
        Just map ->
          ( viewItems map model
          , map.rect
          , if isTopLevel then
              { x = "0", y = "0", w = "1024", h = "600"} -- TODO
            else
              { x = fromInt map.rect.x1
              , y = fromInt map.rect.y1
              , w = fromInt (map.rect.x2 - map.rect.x1)
              , h = fromInt (map.rect.y2 - map.rect.y1)
              }
          )
        Nothing -> (([], []), Rectangle 0 0 0 0, {x = "0", y = "0", w = "0", h = "0"})
  in
  div
    (nestedMapAttributes mapId parentMapId model)
    [ div
        (topicLayerStyle rect)
        topics
    , svg
        ( [ width vb.w
          , height vb.h
          , viewBox (vb.x ++ " " ++ vb.y ++ " " ++ vb.w ++ " " ++ vb.h)
          ]
          ++ svgStyle
        )
        ( assocs
          ++ viewLimboAssoc mapId model
        )
    ]


viewItems : Map -> Model -> ( List (Html Msg), List (Svg Msg) )
viewItems map model =
  map.items |> Dict.values |> List.foldr
    (\{id, hidden, viewProps} (t, a) ->
      if hidden then
        (t, a)
      else
        let
          item = model.items |> Dict.get id
        in
        case (item, viewProps) of
          (Just (Topic topic), ViewTopic props) -> (viewTopic topic props map.id model :: t, a)
          (Just (Assoc assoc), ViewAssoc _) -> (t, viewAssoc assoc map.id model :: a)
          _ -> logError "viewItems" ("problem with item" ++ fromInt id) (t, a)
    )
    ([], [])


viewTopic : TopicInfo -> TopicProps -> MapId -> Model -> Html Msg
viewTopic topic props mapId model =
  div
    ( topicAttr topic.id mapId
      ++ topicStyle topic mapId model
      ++
        case props.displayMode of
          Just BlackBox -> blackBoxStyle topic props
          Just WhiteBox ->
            let
              (rect, offset) =
                case getMap topic.id model.maps of
                  Just map -> (map.rect, map.offset)
                  Nothing -> (Rectangle 0 0 0 0, Offset 0 0)
            in
            whiteboxStyle topic props rect offset
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
        case getMap topicId model.maps of
          Just map -> map.items |> Dict.size
          Nothing -> 0
      else
        0
  in
    [ div
        itemCountStyle
        [ text <| fromInt itemCount ]
    ]


{-| For nested maps give the outer div both the topic meta data and a size. So mouse events
can land there and are detected as mousedown-on-item. Otherwise the target would be the
SVG but our event decoder does not work there.
-}
nestedMapAttributes : MapId -> MapId -> Model -> List (Attribute Msg)
nestedMapAttributes mapId parentMapId model =
  let
    isTopLevel = mapId == model.activeMap
  in
  if isTopLevel then
    []
  else
    topicAttr mapId parentMapId
    ++
    nestedMapStyle


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
            (topicPos topicId mapId model.maps)
            (relPos pos mapId model)
        in
        case points of
          Just (pos1, pos2) -> [ viewLine pos1 pos2 ]
          Nothing -> []
      else
        []
    _ -> []


viewLine : Point -> Point -> Svg Msg
viewLine pos1 pos2 =
  line
    (lineStyle pos1 pos2)
    []


assocGeometry : AssocInfo -> MapId -> Model -> Maybe ( Point, Point )
assocGeometry assoc mapId model =
  let
    pos1 = topicPos assoc.player1 mapId model.maps
    pos2 = topicPos assoc.player2 mapId model.maps
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
    props = ViewTopic (TopicProps pos Nothing)
    pos = Point 112 86
  in
  addItemToMap topicId props model.activeMap newModel


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
    props = ViewAssoc AssocProps
  in
  addItemToMap assocId props mapId newModel


moveTopicToMap : Id -> MapId -> Id -> MapId -> Point -> Model -> Model
moveTopicToMap topicId mapId targetId targetMapId pos model =
  let
    (newModel, created) = createMapIfNeeded targetId targetMapId model
    newPos = if created then Point 0 0 else pos
    viewProps_ = getTopicProps topicId mapId newModel.maps |> Maybe.andThen
      (\props -> Just (ViewTopic { props | pos = newPos }))
  in
  case viewProps_ of
    Just viewProps ->
      addItemToMap topicId viewProps targetId
        { newModel
        | maps = removeItemFromMap topicId mapId newModel
        , selection = [ (targetId, targetMapId) ]
        } |> updateGeometry
    Nothing -> model


createMapIfNeeded : Id -> MapId -> Model -> (Model, Bool)
createMapIfNeeded targetId targetMapId model =
  if isContainer targetId model then
    (model, False)
  else
    ( { model
      | maps = updateDisplayMode targetId targetMapId (Just BlackBox)
          { model
          | maps = model.maps |>
            Dict.insert
              targetId
              (Map targetId Dict.empty (Rectangle 0 0 0 0) (Offset 0 0) targetMapId)
          }
      }
    , True
    )


addItemToMap : Id -> ViewProps -> MapId -> Model -> Model
addItemToMap itemId props mapId model =
  let
    (newModel, assocId) = createAssoc
      "dmx.composition"
      itemId "dmx.child"
      mapId "dmx.parent"
      model
    viewItem = ViewItem itemId False props assocId -- hidden=False
  in
  { newModel | maps =
    updateMaps
      mapId
      (\map -> { map | items = map.items |> Dict.insert itemId viewItem })
      newModel.maps
  }


getSingleSelection : Model -> Maybe (Id, MapId)
getSingleSelection model =
  case model.selection of
    [] -> Nothing
    selItem :: selItems -> Just selItem
    -- FIXME: return nothing if more than one item


updateGeometry : Model -> Model
updateGeometry model =
  { model | maps =
    let
      (_, _, maps) = updateMapGeometry model.activeMap 0 model.maps
    in
    maps
  }


updateMapGeometry : MapId -> Int -> Maps -> (Rectangle, Int, Maps)
updateMapGeometry mapId level maps =
  --
  -- calculate and store the map's "rect" and, based on its change, calculate and store
  -- 1) the map's "pos" adjustmennt ("delta") and 2) the whitebox positioning "offset"
  --
  let
    (rect, level_, maps_) = calcMapRectangle mapId level maps
    maps__ =
      if level == 0 then
        Just maps_
      else
        getMap mapId maps_ |> Maybe.andThen
          (\map -> Just
            ( let
                delta = Point
                  ((rect.x1 + rect.x2 - map.rect.x1 - map.rect.x2) // 2)
                  ((rect.y1 + rect.y2 - map.rect.y1 - map.rect.y2) // 2)
              in
              updateMapRectAndOffset mapId rect delta maps_
                |> updateTopicPos mapId map.parentMapId delta
            )
          )
  in
  case maps__ of
    Just maps___ -> (rect, level_, maps___)
    Nothing -> (rect, level_, maps_)


updateMapRectAndOffset : MapId -> Rectangle -> Delta -> Maps -> Maps
updateMapRectAndOffset mapId rect delta maps =
  updateMaps
    mapId
    (\map ->
      { map
      | rect = rect
      , offset = Offset (map.offset.x - delta.x) (map.offset.y - delta.y)
      }
    )
    maps


calcMapRectangle : MapId -> Int -> Maps -> (Rectangle, Int, Maps)
calcMapRectangle mapId level maps =
  let
    result_ = getMap mapId maps |> Maybe.andThen
      (\map -> Just
        (map.items |> Dict.values |> List.foldr
          (\viewItem (rect, level_, maps_) ->
            case viewItem.viewProps of
              ViewTopic {pos, displayMode} ->
                case displayMode of
                  Just BlackBox -> extent pos blackBoxRect (Offset 0 0) rect level maps_
                  Just WhiteBox ->
                    let
                      (rect_, level__, maps__) =
                        updateMapGeometry viewItem.id (level + 1) maps_
                      offset =
                        case getMap viewItem.id maps__ of
                          Just map_ -> map_.offset
                          Nothing -> Offset 0 0
                    in
                    extent pos rect_ offset rect level maps__
                  Just Unboxed -> extent pos topicRect (Offset 0 0) rect level maps_
                  Nothing -> extent pos topicRect (Offset 0 0) rect level maps_
              ViewAssoc _ -> (rect, level_, maps_)
          )
          (Rectangle 5000 5000 -5000 -5000, level, maps) -- x-min y-min x-max y-max
        )
      )
  in
  case result_ of
    Just result -> result
    Nothing -> (Rectangle 0 0 0 0, level, maps)


extent : Point -> Rectangle -> Offset -> Rectangle -> Int -> Maps -> (Rectangle, Int, Maps)
extent pos rect offset rectAcc level maps =
  ( Rectangle
    (min rectAcc.x1 (pos.x + rect.x1 + offset.x - borderWidth))
    (min rectAcc.y1 (pos.y + rect.y1 + offset.y - borderWidth))
    (max rectAcc.x2 (pos.x + rect.x2 + offset.x + borderWidth))
    (max rectAcc.y2 (pos.y + rect.y2 + offset.y + borderWidth))
  , level
  , maps
  )


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
  { model | maps = maps } |> updateGeometry


updateDisplayMode : Id -> MapId -> Maybe DisplayMode -> Model -> Maps
updateDisplayMode topicId mapId displayMode model =
  updateTopicProps
    topicId
    mapId
    model.maps
    (\props -> { props | displayMode = displayMode })


getDisplayMode : Id -> MapId -> Maps -> Maybe DisplayMode
getDisplayMode topicId mapId maps =
  case getTopicProps topicId mapId maps of
    Just { displayMode } -> displayMode
    Nothing -> fail "getDisplayMode" {topicId = topicId, mapId = mapId} Nothing


boxContainer : Id -> MapId -> Model -> Maps
boxContainer containerId targetMapId model =
  transferContent containerId targetMapId boxItems model


unboxContainer : Id -> MapId -> Model -> Maps
unboxContainer containerId targetMapId model =
  transferContent containerId targetMapId unboxItems model


transferContent : Id -> MapId -> TransferFunc -> Model -> Maps
transferContent containerId targetMapId transferFunc model =
  let
    maps_ = getMap containerId model.maps |> Maybe.andThen
      (\containerMap -> Just
        (updateMaps
          targetMapId
          (\targetMap ->
            { targetMap | items = transferFunc containerMap.items targetMap.items model }
          )
          model.maps
        )
      )
  in
  case maps_ of
    Just maps -> maps
    Nothing -> model.maps


boxItems : ViewItems -> ViewItems -> Model -> ViewItems
boxItems sourceItems targetItems model =
  sourceItems |> Dict.values |> List.foldr
    (\viewItem newItems ->
      let
        items = setHidden viewItem.id True newItems
          |> setHidden viewItem.mapAssocId True
      in
      case getMapIfExists viewItem.id model.maps of
        Just map -> boxItems map.items items model
        Nothing -> items
    )
    targetItems


unboxItems : ViewItems -> ViewItems -> Model -> ViewItems
unboxItems sourceItems targetItems model =
  sourceItems |> Dict.values |> List.foldr
    (\viewItem newItems ->
      let
        (topicItem, abort) = targetTopicItem viewItem targetItems model
        assocItem = targetAssocItem viewItem targetItems
        items = Dict.insert viewItem.id topicItem newItems
          |> Dict.insert viewItem.mapAssocId assocItem
      in
      if abort then
        items
      else
        case getMapIfExists viewItem.id model.maps of
          Just map -> unboxItems map.items items model
          Nothing -> items
    )
    targetItems


targetTopicItem : ViewItem -> ViewItems -> Model -> (ViewItem, Bool)
targetTopicItem viewItem targetItems model =
  case targetItems |> Dict.get viewItem.id of
    Just item ->
      -- abort further unboxing if view item exists (= was unboxed before) and is set to
      -- BlackBox or WhiteBox
      let
        abort =
          case item.viewProps of
            ViewTopic props ->
              case props.displayMode of
                Just BlackBox -> True
                Just WhiteBox -> True
                Just Unboxed -> False
                Nothing -> False
            ViewAssoc _ -> False
      in
      ( { item | hidden = False }, abort )
    Nothing ->
      -- by default (when no view item exists) an unboxed container will also be unboxed
      if isContainer viewItem.id model then
        ( { viewItem | viewProps =
              case viewItem.viewProps of
                ViewTopic props -> ViewTopic { props | displayMode = Just Unboxed }
                ViewAssoc props -> ViewAssoc props
          }
        , False )
      else
        ( viewItem, False )


targetAssocItem : ViewItem -> ViewItems -> ViewItem
targetAssocItem viewItem targetItems =
  case targetItems |> Dict.get viewItem.mapAssocId of
    Just item -> { item | hidden = False }
    Nothing -> ViewItem viewItem.mapAssocId False (ViewAssoc AssocProps) -1 -- hidden=False


setHidden : Id -> Bool -> ViewItems -> ViewItems
setHidden itemId hidden items =
  items |> Dict.update
    itemId
    (\item_ ->
      case item_ of
        Just item -> Just { item | hidden = hidden }
        Nothing -> Nothing
    )


updateTopicPos : Id -> Id -> Delta -> Maps -> Maps
updateTopicPos topicId mapId delta maps =
  updateTopicProps topicId mapId maps
    (\props ->
      { props | pos =
        Point
          (props.pos.x + delta.x)
          (props.pos.y + delta.y)
      }
    )


updateTopicProps : Id -> Id -> Maps -> (TopicProps -> TopicProps) -> Maps
updateTopicProps topicId mapId maps propsFunc =
  updateMaps
    mapId
    (updateTopicProps_ topicId propsFunc)
    maps


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


delete : Model -> Model
delete model =
  let
    maps = updateMaps
      model.activeMap -- FIXME: delete items from other/different maps
      (deleteViewItems model.selection)
      model.maps
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
  updateMaps mapId (removeItemFromMap_ itemId) model.maps


removeItemFromMap_ : Id -> Map -> Map
removeItemFromMap_ itemId map =
  { map | items = map.items |> Dict.remove itemId }


updateMaps : MapId -> (Map -> Map) -> Maps -> Maps
updateMaps mapId mapFunc maps =
  maps |> Dict.update mapId
    (\map_ ->
      case map_ of
        Just map -> Just (mapFunc map)
        Nothing -> illegalMapId "updateMaps" mapId Nothing
    )


{-| Transforms an absolute screen position to a map-relative position.
-}
relPos : Point -> MapId -> Model -> Maybe Point
relPos pos mapId model =
  absMapPos mapId (Point 0 0) model |> Maybe.andThen
    (\posAbs -> Just
      (Point
        (pos.x - posAbs.x + borderWidth)
        (pos.y - posAbs.y + borderWidth)
      )
    )


{-| Recursively calculates the absolute position of a map.
    "posAcc" is the position accumulated so far.
-}
absMapPos : MapId -> Point -> Model -> Maybe Point
absMapPos mapId posAcc model =
  if mapId == model.activeMap then
    Just posAcc
  else
    getMap mapId model.maps
      |> Maybe.andThen
        (\map -> topicPos map.id map.parentMapId model.maps
          |> Maybe.andThen
            (\mapPos_ ->
              absMapPos
                map.parentMapId
                (Point
                  (posAcc.x + mapPos_.x + map.offset.x)
                  (posAcc.y + mapPos_.y + map.offset.y)
                )
                model
            )
        )


-- not called anymore
mapPos : MapId -> Maps -> Maybe Point
mapPos mapId maps =
  getParentMapId mapId maps
    |> Maybe.andThen
      (\parentMapId -> topicPos mapId parentMapId maps
        |> Maybe.andThen (\pos -> Just pos)
      )


-- not called anymore
getParentMapId : MapId -> Maps -> Maybe MapId
getParentMapId mapId maps =
  getMap mapId maps |> Maybe.andThen (\map -> Just map.parentMapId)


topicPos : Id -> MapId -> Maps -> Maybe Point
topicPos topicId mapId maps =
  case getTopicProps topicId mapId maps of
    Just { pos } -> Just pos
    Nothing -> fail "topicPos" {topicId = topicId, mapId = mapId} Nothing


getTopicProps : Id -> MapId -> Maps -> Maybe TopicProps
getTopicProps topicId mapId maps =
  case getViewItemById topicId mapId maps of
    Just viewItem ->
      case viewItem.viewProps of
        ViewTopic props -> Just props
        ViewAssoc _ -> topicMismatch "getTopicProps" topicId Nothing
    Nothing -> fail "getTopicProps" {topicId = topicId, mapId = mapId} Nothing


getViewItemById : Id -> MapId -> Maps -> Maybe ViewItem
getViewItemById itemId mapId maps =
  getMap mapId maps |> Maybe.andThen (getViewItem itemId)


getViewItem : Id -> Map -> Maybe ViewItem
getViewItem itemId map =
  case map.items |> Dict.get itemId of
    Just viewItem -> Just viewItem
    Nothing -> itemNotInMap "getViewItem" itemId map.id Nothing


getMap : MapId -> Maps -> Maybe Map
getMap mapId maps =
  case getMapIfExists mapId maps of
    Just map -> Just map
    Nothing -> illegalMapId "getMap" mapId Nothing


getMapIfExists : MapId -> Maps -> Maybe Map
getMapIfExists mapId maps =
  case maps |> Dict.get mapId of
    Just map -> Just map
    Nothing -> Nothing


isContainer : Id -> Model -> Bool
isContainer topicId model =
  model.maps |> Dict.member topicId


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
            DragTopic -> updateTopicPos id mapId delta model.maps
            DrawAssoc -> model.maps
      in
      { model
        | maps = maps
        , dragState = Drag dragMode id mapId pos target -- update lastPoint
      } |> updateGeometry
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
      Drag _ id mapId _ _ ->
        let
          _ = info "mouseUp" "drag ended w/o target"
          _ = case getTopicProps id mapId model.maps of
            Just props ->
              log "" { id = id, pos = props.pos }
            Nothing -> { id = 0, pos = Point 0 0 }
          _ = case getMapIfExists id model.maps of
            Just map -> log "" { rect = map.rect }
            Nothing -> { rect = Rectangle 0 0 0 0 }
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
  let
    rw = whiteboxRange.width // 2
    rh = whiteboxRange.height // 2
  in
  Random.map2 Point
    (Random.int -rw rw) -- FIXME: include topic radius?
    (Random.int -rh rh) -- FIXME: include topic radius?


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
