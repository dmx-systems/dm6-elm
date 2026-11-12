module TopicMap.TopicMap exposing (bootstrapContent, init, allMapTopics, topicPos, randomPos,
  updateTopicPos, updateTopicPosWith, mapTopicOrNothing, assocGeometry, addTopicAt,
  initLimboMapTopic, initTopicPos, hasMapTopic, fullscreen, lookup, setTopicPos, setRect,
  setScroll, revelationBoxId, revelationBoxPath, landingTarget, toLocalPos, toLocalModelPos)

import Box
import Config as C
import Console
import Env exposing (Env)
import Feature.Crdt
import Feature.CrdtDef as CrdtDef
import Feature.SearchDef exposing (SearchResult(..))
import Feature.Sel as Sel
import Model exposing (Model, Msg)
import ModelBase exposing (..)
import SyncModel exposing (SyncModel)
import TopicMap.TopicMapDef as TopicMapDef exposing (TopicMap, MapTopic)

import Crdt
import Crdt.Edit as Edit

import Dict exposing (Dict)
import Random



{-| Looks up a TopicMap in cache.
Logs an error if TopicMap is missing.
-}
lookup : BoxId -> Model -> Maybe TopicMap
lookup (BoxId (TopicId id)) model =
  case Dict.get id model.cache.topicMap of
    Just topicMap ->
      Just topicMap
    Nothing ->
      Console.logError "TopicMap.TopicMap.lookup" ("Missing TopicMap " ++ id) Nothing


bootstrapContent : Model -> Model
bootstrapContent model =
  model
    |> create (new model.cache.rootBoxId)


-- Dispatch.InitHook
init : BoxId -> Model -> Model
init boxId model =
  let
    _ = Console.info "TopicMap.TopicMap.init" boxId
  in
  model
    |> initTopicMap boxId
    |> initMapTopics boxId


initTopicMap : BoxId -> Model -> Model
initTopicMap (BoxId (TopicId id) as boxId) model =
  if Dict.member id model.cache.topicMap then
    let
      _ = Console.info "TopicMap.TopicMap.initTopicMap"
        ("Box " ++ id ++ " has TopicMap entry already")
    in
    model
  else
    let
      _ = Console.info "TopicMap.TopicMap.initTopicMap"
        ("Creating TopicMap entry for box " ++ id)
    in
    model
      |> create (new boxId)


new : BoxId -> TopicMap
new boxId =
  TopicMap boxId (Rectangle 0 0 0 0) (Point 0 0) Dict.empty


initMapTopics : BoxId -> Model -> Model
initMapTopics boxId model =
  case lookup boxId model of
    Just topicMap ->
      model
        |> Box.topicIds boxId
        |> List.foldl
          (\topicId modelAcc ->
            if Dict.member (toTopicId topicId) topicMap.topics then
              modelAcc
            else
              modelAcc
                |> addMapTopic (initMapTopic topicId boxId modelAcc) boxId
          )
          model
    Nothing ->
      let
        _ = Console.fail "TopicMap.TopicMap.initMapTopics" ("MapTopics can't be inited") Nothing
      in
      model


-- TODO: make it a hook, dispatch from Box.deleteTopic_
{- Removes a topic from TopicMap model:
    - remove MapTopic from all TopicMaps
    - remove TopicMap itself if topic is a box
-}
onDeleteTopic : TopicId -> Model -> Model
onDeleteTopic topicId model =
  model.cache.topicMap
    |> Dict.values
    |> List.foldl
      (\topicMap modelAcc ->
        if Dict.member (toTopicId topicId) topicMap.topics then
          modelAcc
            |> removeMapTopic topicId topicMap.id
        else
          modelAcc
      )
      model


--

{-| Projects the given TopicMap into what is to be rendered based on actual box content.
Note: the box content is the source of truth. TopicMap on the other hand remember everything
once rendered. ### TODO: function name
-}
allMapTopics : TopicMap -> Model -> List MapTopic
allMapTopics topicMap model =
  model
    |> Box.topicIds topicMap.id 
    |> List.filterMap
      (\topicId -> mapTopic_ topicId topicMap)


-- Topic position

{-| Returns the topic position inside a topic map.
Logs an error if TopicMap is missing, or inside TopicMap the MapTopic is missing.
-}
topicPos : TopicId -> BoxId -> Model -> Maybe Point
topicPos topicId boxId model =
  case mapTopic topicId boxId model of
    Just topic -> Just topic.pos
    Nothing ->
      Console.fail "TopicMap.TopicMap.topicPos" (topicId, boxId) Nothing


{-| Transforms a topic position into a new topic position and persists it (CRDT).
Logs an error if TopicMap is missing, or inside TopicMap the MapTopic is missing.
-}
updateTopicPos : TopicId -> BoxId -> (Point -> Point) -> Model -> Model
updateTopicPos topicId boxId transform model =
  updateTopicPosWith topicId boxId
    (\pos -> setTopicPos topicId boxId (transform pos) model)
    model


{-| Transforms a topic position into a new model. Allows the caller e.g. to persist the new
position, or to buffer it in transient model (the geometry buffer as utilized while dragging
a topic).
Logs an error if TopicMap is missing, or inside TopicMap the MapTopic is missing.
-}
updateTopicPosWith : TopicId -> BoxId -> (Point -> Model) -> Model -> Model
updateTopicPosWith topicId boxId transform model =
  case topicPos topicId boxId model of
    Just pos ->
      transform pos
    Nothing ->
      let
        _ = Console.fail "TopicMap.TopicMap.updateTopicPosWith" "Can't update pos"
          (topicId, boxId)
      in
      model


--

assocGeometry : Assoc -> BoxId -> Model -> Maybe (Point, Point)
assocGeometry assoc boxId model =
  let
    pos1 = topicPos assoc.topicId1 boxId model
    pos2 = topicPos assoc.topicId2 boxId model
  in
  case Maybe.map2 (\p1 p2 -> (p1, p2)) pos1 pos2 of
    Just geometry -> Just geometry
    Nothing ->
      Console.fail "TopicMap.TopicMap.assocGeometry" {assoc = assoc, boxId = boxId} Nothing


randomPos : TopicId -> BoxId -> Cmd Msg
randomPos topicId boxId =
  Random.generate
    (Model.TopicMap << TopicMapDef.GotRandomPos topicId boxId)
    pointGen


-- GotRandomPos message handler
addTopicAt : TopicId -> BoxId -> Point -> Env -> Env
addTopicAt topicId boxId pos env =
  env
    |> Env.map (addMapTopic (MapTopic topicId pos Collapsed) boxId)
    |> Env.autoSize


pointGen : Random.Generator Point
pointGen =
  let
    cx = C.topicW2 + C.whiteBoxPadding
    cy = C.topicH2 + C.whiteBoxPadding
    rw = C.whiteBoxRange.w
    rh = C.whiteBoxRange.h
  in
  Random.map2
    (\x y -> Point (cx + x) (cy + y))
    (Random.int 0 rw)
    (Random.int 0 rh)


{-| Initial MapTopic for revealing -}
initMapTopic : TopicId -> BoxId -> Model -> MapTopic
initMapTopic topicId boxId model =
  MapTopic
    topicId
    (initTopicPos boxId model) -- TODO: don't lookup TopicMap again
    Collapsed


initLimboMapTopic : TopicId -> BoxId -> Model -> MapTopic
initLimboMapTopic topicId boxId model =
  MapTopic
    topicId
    (initTopicPos boxId model)
    Expanded


{-| Logs an error if TopicMap is missing. -}
initTopicPos : BoxId -> Model -> Point
initTopicPos boxId model =
  case lookup boxId model of
    Just topicMap ->
      Point
        (C.initTopicPos.x + topicMap.rect.x1 + topicMap.scroll.x)
        (C.initTopicPos.y + topicMap.rect.y1 + topicMap.scroll.y)
    Nothing ->
      Point 0 0 -- error is already logged


{-| Look up a MapTopic for a given BoxId.
Logs an error if TopicMap is missing, or inside TopicMap the MapTopic is missing.
-}
mapTopic : TopicId -> BoxId -> Model -> Maybe MapTopic
mapTopic topicId boxId model =
  lookup boxId model
    |> Maybe.andThen (mapTopic_ topicId)


{-| Look up a MapTopic when you have a TopicMap already.
Logs an error if MapTopic is missing.
-}
mapTopic_ : TopicId -> TopicMap -> Maybe MapTopic
mapTopic_ topicId topicMap =
  case mapTopicOrNothing topicId topicMap of
    Just topic -> Just topic
    Nothing -> Console.logError "TopicMap.TopicMap.mapTopic_" ("Missing MapTopic ("
      ++ Console.toString topicId ++ ") inside (" ++ Console.toString topicMap.id ++ ")")
      Nothing


{-| Look up a MapTopic when you have a TopicMap already.
Returns Nothing if MapTopic is not there (not treated as error).
-}
mapTopicOrNothing : TopicId -> TopicMap -> Maybe MapTopic
mapTopicOrNothing (TopicId id) topicMap =
  case Dict.get id topicMap.topics of
    Just topic -> Just topic
    Nothing -> Nothing


hasMapTopic : TopicId -> BoxId -> Model -> Bool
hasMapTopic topicId boxId model =
  case lookup boxId model of
    Just topicMap -> topicMap.topics |> Dict.member (toTopicId topicId)
    Nothing ->
      Console.fail "TopicMap.TopicMap.hasMapTopic" {topicId = topicId, boxId = boxId} False


{-| Logs an error if TopicMap is missing.
### FIXME: make it a hook?
-}
fullscreen : Model -> Maybe TopicMap
fullscreen model =
  lookup model.boxId model


--

{- The box where to reveal search/traversal results.
-}
revelationBoxId : Model -> Maybe BoxId
revelationBoxId model =
  case revelationBoxPath model of
    Just (boxId :: _) -> Just boxId
    _ -> Nothing


{- The box where to reveal search/traversal results, entire path.
-}
revelationBoxPath : Model -> Maybe BoxPath
revelationBoxPath model =
  case model.search.result of
    Topics _ _ ->
      Just <| Sel.landingBoxPath model
    RelTopics _ _ ->
      case Sel.single model of
        Just (_, boxPath) -> Just boxPath
        Nothing -> Nothing
    NoSearch -> Nothing


{- The landing box as a selection target. For a fullscreen box Nothing is returned.
-}
landingTarget : Model -> Maybe Target
landingTarget model =
  case Sel.landingBoxPath model of
    [] -> Nothing
    [ boxId ] -> Nothing -- The fullscreen box is never selected
    BoxId topicId :: boxPath -> Just (T topicId, boxPath)


--

toLocalPos : Point -> BoxPath -> Model -> Point
toLocalPos clientPos boxPath model =
  let
    pos = toLocalModelPos clientPos boxPath model
  in
  accumulateRect pos (Box.firstId boxPath) model


{-| Transforms a client position to a box-local position.
-}
toLocalModelPos : Point -> BoxPath -> Model -> Point
toLocalModelPos clientPos boxPath model =
  case fullscreen model of
    Just topicMap ->
      let
        -- The box's absolute position is computed first. This involves the positions
        -- of all of its parent boxes. So the box's entire path is needed.
        posAbs = absPos boxPath (Point 0 0) model
      in
      -- Basically the box-local position is computed by substracting the box's absolute
      -- position from the client position. Additionally the fullscreen box's scroll value
      -- must be respected.
      Point
        (clientPos.x - posAbs.x + topicMap.scroll.x)
        (clientPos.y - posAbs.y + topicMap.scroll.y - C.appHeaderHeight)
    Nothing -> Point 0 0


{-| Recursively calculates the absolute position of a box.
"posAcc" is the position accumulated so far.
-}
absPos : BoxPath -> Point -> Model -> Point
absPos boxPath posAcc model =
  case boxPath of
    [ boxId ] -> accumulateRect posAcc boxId model
    boxId :: parentBoxId :: boxIds -> accumulatePos posAcc boxId parentBoxId boxIds model
    [] -> Console.logError "TopicMap.ViewModel.absPos" "boxPath is empty!" (Point 0 0)


accumulatePos : Point -> BoxId -> BoxId -> BoxPath -> Model -> Point
accumulatePos posAcc boxId parentBoxId boxIds model =
  let
    {x, y} = accumulateRect posAcc boxId model
  in
  case topicPos (fromBoxId boxId) parentBoxId model of
    Just boxPos ->
      absPos -- recursion
        (parentBoxId :: boxIds)
        (Point
          (x + boxPos.x - C.topicW2)
          (y + boxPos.y + C.topicH2)
        )
        model
    Nothing -> Point 0 0 -- error is already logged


accumulateRect : Point -> BoxId -> Model -> Point
accumulateRect posAcc boxId model =
  case lookup boxId model of
    Just topicMap ->
      Point
        (posAcc.x - topicMap.rect.x1)
        (posAcc.y - topicMap.rect.y1)
    Nothing -> Point 0 0 -- error is already logged



-- CRDT


create : TopicMap -> Model -> Model
create topicMap model =
  model.crdt.doc
    |> Edit.setKey CrdtDef.refs.topicMap (toBoxId topicMap.id) topicMap
    |> Feature.Crdt.editResult model


addMapTopic : MapTopic -> BoxId -> Model -> Model
addMapTopic topic (BoxId (TopicId boxId)) model =
  model.crdt.doc
    |> Edit.setKey (mapTopicsRef boxId) (toTopicId topic.id) topic
    |> Feature.Crdt.editResult model


removeMapTopic : TopicId -> BoxId -> Model -> Model
removeMapTopic (TopicId topicId) (BoxId (TopicId boxId)) model =
  model.crdt.doc
    |> Edit.removeKey (mapTopicsRef boxId) topicId
    |> Feature.Crdt.editResult model


setTopicPos : TopicId -> BoxId -> Point -> Model -> Model
setTopicPos (TopicId topicId) (BoxId (TopicId boxId)) pos model =
  model.crdt.doc
    |> Edit.set (topicPosRef topicId boxId) pos
    |> Feature.Crdt.editResult model


setRect : BoxId -> Rectangle -> Model -> Model
setRect (BoxId (TopicId boxId)) rect model =
  model.crdt.doc
    |> Edit.set (mapRectRef boxId) rect
    |> Feature.Crdt.editResult model


setScroll : BoxId -> Point -> Model -> Model
setScroll (BoxId (TopicId boxId)) scroll model =
  model.crdt.doc
    |> Edit.set (mapScrollRef boxId) scroll
    |> Feature.Crdt.editResult model


-- Ref composition

mapRef : Id -> Crdt.Ref SyncModel Crdt.Nested TopicMap
mapRef id =
  CrdtDef.refs.topicMap
    |> TopicMapDef.topicMapsDict.key id


mapTopicRef : Id -> Id -> Crdt.Ref SyncModel Crdt.Nested MapTopic
mapTopicRef topicId boxId =
  mapTopicsRef boxId
    |> TopicMapDef.mapTopicsDict.key topicId


topicPosRef : Id -> Id -> Crdt.Ref SyncModel Crdt.Nested Point
topicPosRef topicId boxId =
  mapTopicRef topicId boxId
    |> Crdt.at TopicMapDef.mapTopicDoc.pos


mapTopicsRef : Id -> Crdt.Ref SyncModel (Crdt.DictK Crdt.Nested MapTopic) (Dict Id MapTopic)
mapTopicsRef id =
  mapRef id
    |> Crdt.at TopicMapDef.topicMapDoc.topics


mapRectRef : Id -> Crdt.Ref SyncModel Crdt.Nested Rectangle
mapRectRef id =
  mapRef id
    |> Crdt.at TopicMapDef.topicMapDoc.rect


mapScrollRef : Id -> Crdt.Ref SyncModel Crdt.Nested Point
mapScrollRef id =
  mapRef id
    |> Crdt.at TopicMapDef.topicMapDoc.scroll
