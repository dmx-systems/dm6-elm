module TopicMap.TopicMap exposing (update, create, topics, topicPos, setTopicPos,
  updateTopicPos, mapTopicOrNothing, assocGeometry, addTopic, initLimboMapTopic,
  initTopicPos, hasMapTopic, fullscreen, byId, updateRect, updateScrollPos, revelationBoxId,
  revelationBoxPath, landingTarget)

import Box
import Config as C
import Env exposing (Env)
import Feature.SearchDef exposing (SearchResult(..))
import Feature.Sel as Sel
import Model exposing (Model, Msg)
import ModelBase exposing (..)
import Storage as S
import TopicMap.TopicMapDef as TopicMapDef exposing (TopicMap, MapTopic)
import Undo exposing (UndoModel)
import Utils as U

import Dict
import Random
import String exposing (fromInt)



-- UPDATE


update : TopicMapDef.Msg -> Env -> (UndoModel, Cmd Msg)
update msg ({undoModel} as env) =
  case msg of
    TopicMapDef.AddTopic topicId mapId pos -> addTopic_ topicId mapId pos env
      |> S.store |> Undo.push undoModel


--

create : BoxId -> Model -> Model
create mapId model =
  let
    map = TopicMap mapId (Rectangle 0 0 0 0) (Point 0 0) Dict.empty
  in
  model
    |> create_ map


create_ : TopicMap -> Model -> Model
create_ map ({topicMap} as model) =
  { model | topicMap = topicMap
    |> Dict.insert (toBoxId map.id) map
  }


topics : TopicMap -> Model -> List MapTopic
topics map model =
  Box.topicIds map.id model |> List.foldr
    (\topicId itemsAcc ->
      case mapTopic_ topicId map of
        Just item -> item :: itemsAcc
        Nothing -> itemsAcc
    )
    []


{-| Logs an error if TopicMap does not exist, or topic is not in TopicMap, or ID refers not a
topic (but an association).
-}
topicPos : TopicId -> BoxId -> Model -> Maybe Point
topicPos topicId mapId model =
  case mapTopic topicId mapId model of
    Just { pos } -> Just pos
    Nothing -> U.fail "TopicMap.topicPos" {topicId = topicId, mapId = mapId} Nothing


{-| Logs an error if TopicMap does not exist, or if topic is not in TopicMap -}
setTopicPos : TopicId -> BoxId -> Point -> Model -> Model
setTopicPos topicId mapId pos model =
  model
    |> updateTopicPos topicId mapId
      (\_ -> pos)


{-| Logs an error if TopicMap does not exist, or if topic is not in TopicMap -}
updateTopicPos : TopicId -> BoxId -> (Point -> Point) -> Model -> Model
updateTopicPos topicId mapId transform model =
  model
    |> updateMapTopic topicId mapId
      (\topic -> { topic | pos = transform topic.pos })


{-| Canonical MapTopic transformation.
Logs an error if box does not exist, or topic is not in box, or ID refers not a topic (but
an association).
-}
updateMapTopic : TopicId -> BoxId -> (MapTopic -> MapTopic) -> Model -> Model
updateMapTopic topicId mapId transform model =
  model |> updateTopicMap mapId
    (\map ->
      { map | topics = map.topics |> Dict.update (toTopicId topicId)
        (\maybeTopic ->
          case maybeTopic of
            Just topic -> Just (transform topic)
            Nothing -> U.topicNotFound "TopicMap.updateMapTopic" topicId Nothing
        )
      }
    )


assocGeometry : Assoc -> BoxId -> Model -> Maybe (Point, Point)
assocGeometry assoc mapId model =
  let
    pos1 = topicPos assoc.topicId1 mapId model
    pos2 = topicPos assoc.topicId2 mapId model
  in
  case Maybe.map2 (\p1 p2 -> (p1, p2)) pos1 pos2 of
    Just geometry -> Just geometry
    Nothing -> U.fail "TopicMap.assocGeometry" { assoc = assoc, mapId = mapId } Nothing


addTopic : TopicId -> BoxId -> PosHint -> Model -> (Model, Cmd Msg)
addTopic topicId mapId posHint model =
  if hasMapTopic topicId mapId model then
    (model, Cmd.none)
  else
    if posHint == Random then
      let
        toMsg = Model.TopicMap << TopicMapDef.AddTopic topicId mapId
      in
      (model, Random.generate toMsg pointGen)
    else
      let
        topic = initMapTopic topicId mapId model
        _ = U.info "TopicMap.addTopic" {topicId = topicId, mapId = mapId}
      in
      ( model |> updateTopicMap mapId
          (\map ->
            { map | topics = map.topics |> Dict.insert (toTopicId topicId) topic }
          )
      , Cmd.none
      )


addTopic_ : TopicId -> BoxId -> Point -> Env -> Model
addTopic_ topicId mapId pos ({model} as env) =
  model
    |> updateTopicMap mapId (addTopic__ topicId pos)
    |> Env.autoSize env


addTopic__ : TopicId -> Point -> TopicMap -> TopicMap
addTopic__ topicId pos map =
  { map | topics = map.topics |> Dict.insert (toTopicId topicId)
      (MapTopic topicId pos Collapsed)
  }


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


{-| Initial props for a revealed topic -}
initMapTopic : TopicId -> BoxId -> Model -> MapTopic
initMapTopic topicId mapId model =
  MapTopic
    topicId
    (initTopicPos mapId model)
    Collapsed


initLimboMapTopic : TopicId -> BoxId -> Model -> MapTopic
initLimboMapTopic topicId mapId model =
  MapTopic
    topicId
    (initTopicPos mapId model)
    Expanded


{-| Logs an error if box does not exist. -}
initTopicPos : BoxId -> Model -> Point
initTopicPos mapId model =
  case byId mapId model of
    Just map ->
      Point
        (C.initTopicPos.x + map.rect.x1 + map.scroll.x)
        (C.initTopicPos.y + map.rect.y1 + map.scroll.y)
    Nothing -> Point 0 0 -- error is already logged


{-| Looks up a MapTopic in a TopicMap.
The TopicMap in turn is looked up in Model.
Logs an error if TopicMap is absent, or does not have the item.
-}
mapTopic : TopicId -> BoxId -> Model -> Maybe MapTopic
mapTopic topicId mapId model =
  byId mapId model
    |> Maybe.andThen (mapTopic_ topicId)


{-| Looks up a MapTopic in a TopicMap.
Logs an error if the topic map is absent, or does not have the item.
-}
mapTopic_ : TopicId -> TopicMap -> Maybe MapTopic
mapTopic_ topicId map =
  case mapTopicOrNothing topicId map of
    Just topic -> Just topic
    Nothing -> U.logError "TopicMap.TopicMap.mapTopic_"
      ("Missing MapTopic " ++ U.toString topicId ++ " in " ++ fromInt (toBoxId map.id)) Nothing


mapTopicOrNothing : TopicId -> TopicMap -> Maybe MapTopic
mapTopicOrNothing topicId map =
  case map.topics |> Dict.get (toTopicId topicId) of
    Just topic -> Just topic
    Nothing -> Nothing


hasMapTopic : TopicId -> BoxId -> Model -> Bool
hasMapTopic topicId mapId model =
  case byId mapId model of
    Just map -> map.topics |> Dict.member (toTopicId topicId)
    Nothing -> U.fail "TopicMap.hasMapTopic" {topicId = topicId, mapId = mapId} False


{-| Logs an error if TopicMap does not exist.
### FIXME: support other renderers
-}
fullscreen : Model -> Maybe TopicMap
fullscreen model =
  byId model.boxId model


{-| Logs an error if TopicMap does not exist. -}
byId : BoxId -> Model -> Maybe TopicMap
byId mapId model =
  case model.topicMap |> Dict.get (toBoxId mapId) of
    Just map -> Just map
    Nothing -> U.boxNotFound "TopicMap.byId" mapId Nothing


--

updateRect : BoxId -> (Rectangle -> Rectangle) -> Model -> Model
updateRect mapId transform model =
  model |> updateTopicMap mapId
    (\map ->
      { map | rect = transform map.rect }
    )


updateScrollPos : BoxId -> (Point -> Point) -> Model -> Model
updateScrollPos mapId transform model =
  model |> updateTopicMap mapId
    (\map ->
      { map | scroll = transform map.scroll }
    )


{-| Canonical TopicMap transformation.
Logs an error if TopicMap does not exist.
-}
updateTopicMap : BoxId -> (TopicMap -> TopicMap) -> Model -> Model
updateTopicMap mapId transform ({topicMap} as model) =
  { model | topicMap = topicMap
    |> Dict.update (toBoxId mapId)
      (\maybeMap ->
        case maybeMap of
          Just map -> Just (transform map)
          Nothing -> U.boxNotFound "TopicMap.updateTopicMap" mapId Nothing
      )
  }


--

{- The box where to reveal search/traversal results -}
revelationBoxId : Model -> Maybe BoxId
revelationBoxId model =
  case revelationBoxPath model of
    Just (boxId :: _) -> Just boxId
    _ -> Nothing


{- The box where to reveal search/traversal results, entire path -}
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


{- The landing box as a selection target. For a fullscreen box Nothing is returned. -}
landingTarget : Model -> Maybe Target
landingTarget model =
  case Sel.landingBoxPath model of
    [] -> Nothing
    [ boxId ] -> Nothing -- The fullscreen box is never selected
    BoxId topicId :: boxPath -> Just (T topicId, boxPath)
