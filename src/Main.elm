module Main exposing (..)

import Model exposing (..)
import Style exposing (..)
import EditDialog exposing (..)

import Array
import Browser
import Browser.Events as Events
import Dict exposing (Dict)
import Html exposing (Html, Attribute, div, text, button, input, label, h1)
import Html.Attributes exposing (class, attribute, type_, name, checked, disabled)
import Html.Events exposing (onClick, on)
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
      <| Map 0 Dict.empty (Rectangle 0 0 0 0) -1 -- parentMapId = -1
    , activeMap = 0
    , selection = []
    , dragState = NoDrag
    , isEditDialogOpen = False
    , nextId = 1
    }
  , Cmd.none
  )



-- VIEW


view : Model -> Browser.Document Msg
view model =
  Browser.Document
    "DM6 Elm"
    [ div
      ( [ on "mouseover" (mouseDecoder Over)
        , on "mouseout" (mouseDecoder Out)
        ]
        ++ appStyle
      )
      ( [ h1 [] [ text "DM6 Elm" ]
        , viewToolbar model
        , viewMap model.activeMap -1 model -- top-level map has parentMapId -1
        ]
        ++
          if model.isEditDialogOpen then
            [ viewEditDialog model ]
          else
            []
      )
    ]


viewToolbar : Model -> Html Msg
viewToolbar model =
  let
    hasNoSelection = List.isEmpty model.selection
  in
  div
    toolbarStyle
    [ button
        ( [ onClick AddTopic ]
          ++ buttonStyle
        )
        [ text "Add Topic" ]
    , viewDisplayMode model
    , button
        ( [ onClick (Edit Open)
          , stopPropagationOnMousedown
          , disabled hasNoSelection
          ]
          ++ buttonStyle
        )
        [ text "Edit" ]
    , button
        ( [ onClick Delete
          , stopPropagationOnMousedown
          , disabled hasNoSelection
          ]
          ++ buttonStyle
        )
        [ text "Delete" ]
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
        [ text "Container Display" ]
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
              { x = map.rect.x1 |> round |> fromInt
              , y = map.rect.y1 |> round |> fromInt
              , w = (map.rect.x2 - map.rect.x1) |> round |> fromInt
              , h = (map.rect.y2 - map.rect.y1) |> round |> fromInt
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
          _ -> logError "viewItems" ("problem with item " ++ fromInt id) (t, a)
    )
    ([], [])


viewTopic : TopicInfo -> TopicProps -> MapId -> Model -> Html Msg
viewTopic topic props mapId model =
  let
    (style, children) =
      case props.displayMode of
        Just BlackBox -> (blackBoxStyle topic props, viewItemCount topic.id props model)
        Just WhiteBox ->
          ( let
              rect =
                case getMap topic.id model.maps of
                  Just map -> map.rect
                  Nothing -> Rectangle 0 0 0 0
            in
            whiteboxStyle topic props rect
          , [ viewMap topic.id mapId model ]
          )
        Just Unboxed -> normalTopic topic props model
        Nothing -> normalTopic topic props model
  in
  div
    ( topicAttr topic.id mapId
      ++ topicStyle topic mapId model
      ++ style
    )
    children


normalTopic : TopicInfo -> TopicProps -> Model -> (List (Attribute Msg), List (Html Msg))
normalTopic topic props model =
  ( normalStyle topic props
  , [ div
      topicIconBoxStyle
      [ viewTopicIcon topic.id model ]
    ]
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
    Drag DrawAssoc topicId mapId_ _ pos _ ->
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
  case Maybe.map2 (\p1 p2 -> ( p1, p2 )) pos1 pos2 of
    Just geometry -> Just geometry
    Nothing -> fail "assocGeometry" { assoc = assoc, mapId = mapId } Nothing



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
    AddTopic -> (createTopicAndAddToMap model, Cmd.none)
    MoveTopicToMap topicId mapId origPos targetId targetMapId pos
      -> (moveTopicToMap topicId mapId origPos targetId targetMapId pos model, Cmd.none)
    Set displayMode -> (setDisplayMode displayMode model, Cmd.none)
    Edit editMsg -> (updateEditDialog editMsg model, Cmd.none)
    Mouse mouseMsg -> updateMouse mouseMsg model
    Delete -> (delete model, Cmd.none)
    NoOp -> (model, Cmd.none)


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
    | items = model.items |> Dict.insert id ( Topic <| TopicInfo id color Nothing )
    , nextId = id + 1
    }
  , id
  )


createTopicAndAddToMap : Model -> Model
createTopicAndAddToMap model =
  let
    (newModel, topicId) = createTopic model
    props = ViewTopic (TopicProps pos Nothing)
    pos = Point 160 98
  in
  newModel
    |> addItemToMap topicId props model.activeMap
    |> select topicId model.activeMap


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


moveTopicToMap : Id -> MapId -> Point -> Id -> MapId -> Point -> Model -> Model
moveTopicToMap topicId mapId origPos targetId targetMapId pos model =
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
        | maps = hideItem topicId mapId newModel.maps model |> setTopicPos topicId mapId origPos
        , selection = [ (targetId, targetMapId) ] -- TODO: call "select"
        } |> updateGeometry
    Nothing -> model


createMapIfNeeded : Id -> MapId -> Model -> (Model, Bool)
createMapIfNeeded targetId targetMapId model =
  if hasMap targetId model.maps then
    (model, False)
  else
    ( { model
      | maps = updateDisplayMode targetId targetMapId (Just BlackBox)
          { model
          | maps = model.maps |>
            Dict.insert
              targetId
              (Map targetId Dict.empty (Rectangle 0 0 0 0) targetMapId)
          }
      }
    , True
    )


addItemToMap : Id -> ViewProps -> MapId -> Model -> Model
addItemToMap itemId props mapId model =
  let
    (newModel, parentAssocId) = createAssoc
      "dmx.composition"
      itemId "dmx.child"
      mapId "dmx.parent"
      model
    viewItem = ViewItem itemId False props parentAssocId -- hidden=False
    _ = info "addItemToMap"
      { itemId = itemId, props = props, mapId = mapId, parentAssocId = parentAssocId}
  in
  { newModel | maps =
    updateMaps
      mapId
      (\map -> { map | items = map.items |> Dict.insert itemId viewItem })
      newModel.maps
  }


updateGeometry : Model -> Model
updateGeometry model =
  { model | maps =
    let
      (_, _, maps) = updateMapGeometry model.activeMap 0 model.maps
    in
    maps
  }


-- called indirect recursive
updateMapGeometry : MapId -> Int -> Maps -> (Rectangle, Int, Maps)
updateMapGeometry mapId level maps =
  -- 1) calculate and store the map's "rect" and, based on its change,
  -- 2) calculate and store the map's "pos" adjustmennt ("delta")
  let
    (rect, level_, maps_) = calcMapRect mapId level maps
    maps__ =
      if level == 0 then
        maps_
      else
        case getMap mapId maps_ of
          Just map ->
            ( let
                delta = Point
                  ((rect.x1 + rect.x2 - map.rect.x1 - map.rect.x2) / 2)
                  ((rect.y1 + rect.y2 - map.rect.y1 - map.rect.y2) / 2)
              in
              updateMapRect mapId rect maps_
                |> updateTopicPos mapId map.parentMapId delta
            )
          Nothing -> maps_
  in
  (rect, level_, maps__)


updateMapRect : MapId -> Rectangle -> Maps -> Maps
updateMapRect mapId rect maps =
  updateMaps
    mapId
    (\map -> { map | rect = rect })
    maps


calcMapRect : MapId -> Int -> Maps -> (Rectangle, Int, Maps)
calcMapRect mapId level maps =
  let
    result_ = getMap mapId maps |> Maybe.andThen
      (\map -> Just
        (map.items |> Dict.values |> List.foldr
          (\viewItem (rect, level_, maps_) ->
            if not viewItem.hidden then
              calcItemSize viewItem rect level_ maps_
            else
              (rect, level_, maps_)
          )
          (Rectangle 5000 5000 -5000 -5000, level, maps) -- x-min y-min x-max y-max
        )
      )
  in
  case result_ of
    Just result -> result
    Nothing -> (Rectangle 0 0 0 0, level, maps)


calcItemSize : ViewItem -> Rectangle -> Int -> Maps -> (Rectangle, Int, Maps)
calcItemSize viewItem rect level maps =
  let
    (rect__, maps__) =
      case viewItem.viewProps of
        ViewTopic {pos, displayMode} ->
          case displayMode of
            Just BlackBox -> (extent pos blackBoxRect rect, maps)
            Just WhiteBox ->
              let
                (rect_, _, maps_) = updateMapGeometry viewItem.id (level + 1) maps
              in
              (extent pos rect_ rect, maps_)
            Just Unboxed -> (extent pos topicRect rect, maps)
            Nothing -> (extent pos topicRect rect, maps)
        ViewAssoc _ -> (rect, maps)
  in
  (rect__, level, maps__)


extent : Point -> Rectangle -> Rectangle -> Rectangle
extent pos rect rectAcc =
  let
    w2 = (rect.x2 - rect.x1) / 2
    h2 = (rect.y2 - rect.y1) / 2
  in
  Rectangle
    (min rectAcc.x1 (pos.x - w2 - whiteboxPadding))
    (min rectAcc.y1 (pos.y - h2 - whiteboxPadding))
    (max rectAcc.x2 (pos.x + w2 + whiteboxPadding))
    (max rectAcc.y2 (pos.y + h2 + whiteboxPadding))


setDisplayMode : Maybe DisplayMode -> Model -> Model
setDisplayMode displayMode model =
  let
    maps =
      case getSingleSelection model of
        Just (containerId, targetMapId) ->
          let
            newModel =
              { model | maps =
                  case displayMode of
                    Just BlackBox -> boxContainer containerId targetMapId model
                    Just WhiteBox -> boxContainer containerId targetMapId model
                    Just Unboxed -> unboxContainer containerId targetMapId model
                    Nothing -> model.maps
              }
          in
          updateDisplayMode containerId targetMapId displayMode newModel
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


{-| Entry point
-}
boxContainer : Id -> MapId -> Model -> Maps
boxContainer containerId targetMapId model =
  transferContent containerId targetMapId boxItems model


{-| Entry point
-}
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


{-| Transfer function, Boxing.
Iterates the container items (recursively) and sets corresponding target items to hidden.
Returns the updated target items.
-}
boxItems : ViewItems -> ViewItems -> Model -> ViewItems
boxItems containerItems targetItems model =
  containerItems |> Dict.values |> List.foldr
    (\containerItem targetItemsAcc ->
      let
        items = hideItems containerItem.id model targetItemsAcc
      in
      case getMapIfExists containerItem.id model.maps of
        Just map -> boxItems map.items items model
        Nothing -> items
    )
    targetItems


{-| Transfer function, Unboxing.
Iterates the container items (recursively) and reveals corresponding target items.
Returns the updated target items.
-}
unboxItems : ViewItems -> ViewItems -> Model -> ViewItems
unboxItems containerItems targetItems model =
  containerItems |> Dict.values |> List.foldr
    (\containerItem targetItemsAcc ->
      if isTopic containerItem then
        let
          (items, abort) = unboxTopic containerItem targetItemsAcc model
        in
        if not abort then
          case getMapIfExists containerItem.id model.maps of
            Just map -> unboxItems map.items items model
            Nothing -> items
        else
          items
      else
        unboxAssoc containerItem targetItemsAcc
    )
    targetItems


{-| Returns the target item to reveal that corresponds to the container item.
Part of unboxing. FIXDOC
-}
unboxTopic : ViewItem -> ViewItems -> Model -> (ViewItems, Bool)
unboxTopic containerItem targetItems model =
  let
    (topicToInsert, abort) =
      case targetItems |> Dict.get containerItem.id of
        Just item ->
          -- abort further unboxing if view item exists (= was unboxed before) and is set to
          -- BlackBox or WhiteBox
          ({ item | hidden = False }, isAbort item)
        Nothing ->
          -- by default (when no view item exists) an unboxed container will also be unboxed
          if hasMap containerItem.id model.maps then
            (setUnboxed containerItem, False)
          else
            (containerItem, False)
    assocToInsert = targetAssocItem containerItem.parentAssocId targetItems
  in
  ( targetItems
    |> Dict.insert topicToInsert.id topicToInsert
    |> Dict.insert assocToInsert.id assocToInsert
  , abort
  )


unboxAssoc : ViewItem -> ViewItems -> ViewItems
unboxAssoc containerItem targetItems =
  let
    assocToInsert = targetAssocItem containerItem.id targetItems
  in
  targetItems
    |> Dict.insert assocToInsert.id assocToInsert


setUnboxed : ViewItem -> ViewItem
setUnboxed item =
  { item | viewProps =
    case item.viewProps of
      ViewTopic props -> ViewTopic { props | displayMode = Just Unboxed }
      ViewAssoc props -> ViewAssoc props
  }


isAbort : ViewItem -> Bool
isAbort item =
  case item.viewProps of
    ViewTopic props ->
      case props.displayMode of
        Just BlackBox -> True
        Just WhiteBox -> True
        Just Unboxed -> False
        Nothing -> False
    ViewAssoc _ -> False


{-| Returns the target item to reveal that corresponds to the container item.
Part of unboxing. FIXDOC
-}
targetAssocItem : Id -> ViewItems -> ViewItem
targetAssocItem assocId targetItems =
  case targetItems |> Dict.get assocId of
    Just item -> { item | hidden = False }
    Nothing -> ViewItem assocId False (ViewAssoc AssocProps) -1


hideItem : Id -> MapId -> Maps -> Model -> Maps
hideItem itemId mapId maps model =
  updateMaps
    mapId
    (\map -> { map | items = hideItems itemId model map.items })
    maps


hideItems : Id -> Model -> ViewItems -> ViewItems
hideItems itemId model items =
  let
    newItems = items |> Dict.update
      itemId
      (\item_ ->
        case item_ of
          Just item -> Just { item | hidden = True }
          Nothing -> Nothing
      )
    assocIds = assocsOfPlayer itemId items model
  in
  List.foldr
    (\assocId newItems_ -> hideItems assocId model newItems_)
    newItems
    assocIds


assocsOfPlayer : Id -> ViewItems -> Model -> List Id
assocsOfPlayer itemId items model =
  items |> Dict.values
    |> List.filter isAssoc
    |> List.filter (hasPlayer itemId model)
    |> List.map .id


isTopic : ViewItem -> Bool
isTopic item =
  case item.viewProps of
    ViewTopic _ -> True
    ViewAssoc _ -> False


isAssoc : ViewItem -> Bool
isAssoc item =
  case item.viewProps of
    ViewTopic _ -> False
    ViewAssoc _ -> True


hasPlayer : Id -> Model -> ViewItem -> Bool
hasPlayer itemId model assocItem =
  case getAssocInfo assocItem.id model of
    Just assoc -> assoc.player1 == itemId || assoc.player2 == itemId
    Nothing -> False


setTopicPos : Id -> Id -> Point -> Maps -> Maps
setTopicPos topicId mapId pos maps =
  updateTopicProps topicId mapId maps
    (\props -> { props | pos = pos })


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
            ViewAssoc _ -> topicMismatch "updateTopicProps" topicId Nothing
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


removeItemFromMap : Id -> MapId -> Maps -> Maps
removeItemFromMap itemId mapId maps =
  updateMaps mapId (removeItemFromMap_ itemId) maps


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
        (pos.x - posAbs.x)
        (pos.y - posAbs.y)
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
                  (posAcc.x + mapPos_.x - (map.rect.x2 + map.rect.x1) / 2)
                  (posAcc.y + mapPos_.y - (map.rect.y2 + map.rect.y1) / 2)
                )
                model
            )
        )


-- not called
mapPos : MapId -> Maps -> Maybe Point
mapPos mapId maps =
  getParentMapId mapId maps
    |> Maybe.andThen
      (\parentMapId -> topicPos mapId parentMapId maps
        |> Maybe.andThen (\pos -> Just pos)
      )


-- not called
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


hasMap : MapId -> Maps -> Bool
hasMap mapId maps =
  maps |> Dict.member mapId


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
    Time time -> ( timeArrived time model, Cmd.none )


mouseDown : Model -> Model
mouseDown model =
  { model | selection = [] }


mouseDownOnItem : Model -> Class -> Id -> MapId -> Point -> ( Model, Cmd Msg )
mouseDownOnItem model class id mapId pos =
  ( { model
    | selection = [ (id, mapId) ] -- TODO: call "select"
    , dragState = WaitForStartTime class id mapId pos
    }
  , Task.perform (Time >> Mouse) Time.now
  )


timeArrived : Time.Posix -> Model -> Model
timeArrived time model =
  case model.dragState of
    WaitForStartTime class id mapId pos ->
      { model | dragState = DragEngaged time class id mapId pos }
    WaitForEndTime startTime class id mapId pos ->
      { model | dragState =
        case class of
          "dmx-topic" ->
            let
              delay = posixToMillis time - posixToMillis startTime > dragThresholdMillis
              dragMode = if delay then DrawAssoc else DragTopic
              origPos_ = topicPos id mapId model.maps
            in
            case origPos_ of
              Just origPos -> Drag dragMode id mapId origPos pos Nothing
              Nothing -> NoDrag
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
    Drag _ _ _ _ _ _ ->
      ( performDrag model pos, Cmd.none )
    _ -> logError "mouseMove"
      ("Received \"Move\" message when dragState is " ++ toString model.dragState)
      ( model, Cmd.none )


performDrag : Model -> Point -> Model
performDrag model pos =
  case model.dragState of
    Drag dragMode id mapId origPos lastPos target ->
      let
        delta = Point
          (pos.x - lastPos.x)
          (pos.y - lastPos.y)
        maps =
          case dragMode of
            DragTopic -> updateTopicPos id mapId delta model.maps
            DrawAssoc -> model.maps
      in
      { model
        | maps = maps
        , dragState = Drag dragMode id mapId origPos pos target -- update lastPos
      } |> updateGeometry
    _ -> logError "performDrag"
      ("Received \"Move\" message when dragState is " ++ toString model.dragState)
      model


mouseUp : Model -> ( Model, Cmd Msg )
mouseUp model =
  let
    (newModel, cmd) =
      case model.dragState of
        Drag DragTopic id mapId origPos _ (Just (targetId, targetMapId)) ->
          let
            _ = info "mouseUp" ("dropped " ++ fromInt id ++ " (map " ++ fromInt mapId
              ++ ") on " ++ fromInt targetId ++ " (map " ++ fromInt targetMapId ++ ") --> "
              ++ if notDroppedOnOwnMap then "move topic" else "abort")
            notDroppedOnOwnMap = mapId /= targetId
            msg = MoveTopicToMap id mapId origPos targetId targetMapId
          in
          if notDroppedOnOwnMap then
            (model, Random.generate msg point)
          else
            (model, Cmd.none)
        Drag DrawAssoc id mapId _ _ (Just (targetId, targetMapId)) ->
          let
            _ = info "mouseUp" ("assoc drawn from " ++ fromInt id ++ " (map " ++ fromInt mapId
              ++ ") to " ++ fromInt targetId ++ " (map " ++ fromInt targetMapId ++ ") --> "
              ++ if isSameMap then "create assoc" else "abort")
            isSameMap = mapId == targetMapId
          in
          if isSameMap then
            (createDefaultAssoc id targetId mapId model, Cmd.none)
          else
            (model, Cmd.none)
        Drag _ id mapId _ _ _ ->
          let
            _ = info "mouseUp" "drag ended w/o target"
            {- _ = case getTopicProps id mapId model.maps of
              Just props ->
                log "" { id = id, pos = props.pos }
              Nothing -> { id = 0, pos = Point 0 0 }
            _ = case getMapIfExists id model.maps of
              Just map -> log "" { rect = map.rect }
              Nothing -> { rect = Rectangle 0 0 0 0 } -}
          in
          (model, Cmd.none)
        DragEngaged _ _ _ _ _ ->
          let
            _ = info "mouseUp" "drag aborted w/o moving"
          in
          (model, Cmd.none)
        _ ->
          logError "mouseUp"
            ("Received \"Up\" message when dragState is " ++ toString model.dragState)
            (model, Cmd.none)
  in
  ({ newModel | dragState = NoDrag }, cmd)


point : Random.Generator Point
point =
  let
    rw = whiteboxRange.width / 2
    rh = whiteboxRange.height / 2
  in
  Random.map2 Point
    (Random.float -rw rw)
    (Random.float -rh rh)


mouseOver : Model -> Class -> Id -> MapId -> Model
mouseOver model class targetId targetMapId =
  case model.dragState of
    Drag dragMode id mapId origPos lastPos _ ->
      let
        target =
          if (id, mapId) /= (targetId, targetMapId) then
            Just (targetId, targetMapId)
          else
            Nothing
      in
      { model | dragState = Drag dragMode id mapId origPos lastPos target } -- update target
    DragEngaged _ _ _ _ _ ->
      logError "mouseOver" "Received \"Over\" message when dragState is DragEngaged" model
    _ -> model


mouseOut : Model -> Class -> Id -> MapId -> Model
mouseOut model class targetId targetMapId =
  case model.dragState of
    Drag dragMode id mapId origPos lastPos _ ->
      { model | dragState = Drag dragMode id mapId origPos lastPos Nothing } -- reset target
    _ -> model



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
  case model.dragState of
    NoDrag -> mouseDownSub
    WaitForStartTime _ _ _ _ -> Sub.none
    WaitForEndTime _ _ _ _ _ -> Sub.none
    DragEngaged _ _ _ _ _ -> dragSub
    Drag _ _ _ _ _ _ -> dragSub


mouseDownSub : Sub Msg
mouseDownSub =
  Events.onMouseDown <| D.oneOf
    [ D.map Mouse <| D.map4 DownItem
        ( D.at ["target", "className"] D.string )
        ( D.at ["target", "dataset", "id"] D.string |> D.andThen strToIntDecoder )
        ( D.at ["target", "dataset", "mapId"] D.string |> D.andThen strToIntDecoder )
        ( D.map2 Point -- TODO: no code doubling
          ( D.field "clientX" D.float )
          ( D.field "clientY" D.float )
        )
    , D.succeed (Mouse Down)
    ]


dragSub : Sub Msg
dragSub =
  Sub.batch
    [ Events.onMouseMove <| D.map Mouse <| D.map Move
        ( D.map2 Point -- TODO: no code doubling
          ( D.field "clientX" D.float )
          ( D.field "clientY" D.float )
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
