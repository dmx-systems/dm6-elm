module TopicMap.TopicMap exposing (init, allMapTopics, topicPos, randomPos, setTopicPos,
  updateTopicPos, mapTopicOrNothing, assocGeometry, addTopicAt, initLimboMapTopic, initTopicPos,
  hasMapTopic, fullscreen, byId, updateRect, updateScrollPos, revelationBoxId,
  revelationBoxPath, landingTarget, toLocalPos, toLocalModelPos)

import Box
import Config as C
import Console
import Env exposing (Env)
import Feature.SearchDef exposing (SearchResult(..))
import Feature.Sel as Sel
import Model exposing (Model, Msg)
import ModelBase exposing (..)
import TopicMap.TopicMapDef as TopicMapDef exposing (TopicMap, MapTopic)

import Dict
import Random



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
initTopicMap boxId ({topicMap} as model) =
  let
    id = toBoxId boxId
  in
  if Dict.member id topicMap.view then
    let
      _ = Console.info "TopicMap.TopicMap.initTopicMap"
        ("Box (" ++ Console.toString boxId ++ ") has TopicMap entry already")
    in
    model
  else
    let
      _ = Console.info "TopicMap.TopicMap.initTopicMap"
        ("Creating TopicMap entry for box (" ++ Console.toString boxId ++ ")")
      topicMap_ = TopicMap boxId (Rectangle 0 0 0 0) (Point 0 0) Dict.empty
    in
    { model | topicMap =
      { topicMap | view = topicMap.view
          |> Dict.insert id topicMap_
      }
    }


initMapTopics : BoxId -> Model -> Model
initMapTopics boxId model =
  model
    |> updateTopicMap boxId
      (\topicMap ->
        { topicMap | topics =
            Box.topicIds boxId model
              |> List.foldl
                (\topicId acc ->
                  let
                    id = toTopicId topicId
                  in
                  if Dict.member id acc then
                    acc
                  else
                    acc |> Dict.insert id (initMapTopic topicId boxId model)
                )
                topicMap.topics
        }
      )


--

{-| Projects the given TopicMap into what is to be rendered based on actual box content.
Note: the box content is the source of truth. TopicMap on the other hand remember everything
once rendered. ### TODO: function name
-}
allMapTopics : TopicMap -> Model -> List MapTopic
allMapTopics topicMap model =
  Box.topicIds topicMap.id model
    |> List.filterMap
      (\topicId -> mapTopic_ topicId topicMap)


{-| Logs an error if the TopicMap entry is missing, or inside TopicMap the MapTopic entry is
missing.
-}
topicPos : TopicId -> BoxId -> Model -> Maybe Point
topicPos topicId boxId model =
  case mapTopic topicId boxId model of
    Just {pos} -> Just pos
    Nothing ->
      Console.fail "TopicMap.TopicMap.topicPos" {topicId = topicId, boxId = boxId} Nothing


{-| Logs an error if the TopicMap entry is missing, or inside TopicMap the MapTopic entry is
missing. -}
setTopicPos : TopicId -> BoxId -> Point -> Model -> Model
setTopicPos topicId boxId pos model =
  model
    |> updateTopicPos topicId boxId (\_ -> pos)


{-| Logs an error if the TopicMap entry is missing, or inside TopicMap the MapTopic entry is
missing. -}
updateTopicPos : TopicId -> BoxId -> (Point -> Point) -> Model -> Model
updateTopicPos topicId boxId transform model =
  model
    |> updateMapTopic topicId boxId
      (\topic -> { topic | pos = transform topic.pos })


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


randomPos : TopicId -> BoxId -> Model -> (Model, Cmd Msg)
randomPos topicId boxId model =
  let
    toMsg = Model.TopicMap << TopicMapDef.GotRandomPos topicId boxId
    cmd = Random.generate toMsg pointGen
  in
  (model, cmd)


-- GotRandomPos message handler
addTopicAt : TopicId -> BoxId -> Point -> Env -> Env
addTopicAt topicId boxId pos env =
  env
    |> Env.map (updateTopicMap boxId (addTopic_ topicId pos))
    |> Env.autoSize


addTopic_ : TopicId -> Point -> TopicMap -> TopicMap
addTopic_ topicId pos topicMap =
  { topicMap | topics =
      topicMap.topics |> Dict.insert
        (toTopicId topicId)
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


{-| Initial MapTopic for revealing -}
initMapTopic : TopicId -> BoxId -> Model -> MapTopic
initMapTopic topicId boxId model =
  MapTopic
    topicId
    (initTopicPos boxId model)
    Collapsed


initLimboMapTopic : TopicId -> BoxId -> Model -> MapTopic
initLimboMapTopic topicId boxId model =
  MapTopic
    topicId
    (initTopicPos boxId model)
    Expanded


{-| Logs an error if the TopicMap entry is missing. -}
initTopicPos : BoxId -> Model -> Point
initTopicPos boxId model =
  case byId boxId model of
    Just topicMap ->
      Point
        (C.initTopicPos.x + topicMap.rect.x1 + topicMap.scroll.x)
        (C.initTopicPos.y + topicMap.rect.y1 + topicMap.scroll.y)
    Nothing -> Point 0 0 -- error is already logged


{-| Looks up MapTopic for a given BoxId.
Logs an error if the TopicMap entry is missing, or inside TopicMap the MapTopic entry is
missing.
-}
mapTopic : TopicId -> BoxId -> Model -> Maybe MapTopic
mapTopic topicId boxId model =
  byId boxId model
    |> Maybe.andThen (mapTopic_ topicId)


{-| Looks up MapTopic inside the given TopicMap.
Logs an error if the MapTopic entry is missing.
-}
mapTopic_ : TopicId -> TopicMap -> Maybe MapTopic
mapTopic_ topicId topicMap =
  case mapTopicOrNothing topicId topicMap of
    Just topic -> Just topic
    Nothing -> Console.logError "TopicMap.TopicMap.mapTopic_" ("Missing MapTopic ("
      ++ Console.toString topicId ++ ") inside (" ++ Console.toString topicMap.id ++ ")")
      Nothing


mapTopicOrNothing : TopicId -> TopicMap -> Maybe MapTopic
mapTopicOrNothing topicId topicMap =
  case topicMap.topics |> Dict.get (toTopicId topicId) of
    Just topic -> Just topic
    Nothing -> Nothing


hasMapTopic : TopicId -> BoxId -> Model -> Bool
hasMapTopic topicId boxId model =
  case byId boxId model of
    Just topicMap -> topicMap.topics |> Dict.member (toTopicId topicId)
    Nothing ->
      Console.fail "TopicMap.TopicMap.hasMapTopic" {topicId = topicId, boxId = boxId} False


{-| Logs an error if the TopicMap entry is missing.
### FIXME: make it a hook?
-}
fullscreen : Model -> Maybe TopicMap
fullscreen model =
  byId model.boxId model


{-| Logs an error if the TopicMap entry is missing.
-}
byId : BoxId -> Model -> Maybe TopicMap
byId boxId model =
  case model.topicMap.view |> Dict.get (toBoxId boxId) of
    Just topicMap -> Just topicMap
    Nothing -> Console.logError "TopicMap.TopicMap.byId"
      ("Missing TopicMap entry for (" ++ Console.toString boxId ++ ")") Nothing


--

updateRect : BoxId -> (Rectangle -> Rectangle) -> Model -> Model
updateRect boxId transform model =
  model
    |> updateTopicMap boxId
      (\topicMap ->
        { topicMap | rect = transform topicMap.rect }
      )


updateScrollPos : BoxId -> (Point -> Point) -> Model -> Model
updateScrollPos boxId transform model =
  model
    |> updateTopicMap boxId
      (\topicMap ->
        { topicMap | scroll = transform topicMap.scroll }
      )


{-| Canonical MapTopic transformation.
Logs an error if the TopicMap entry is missing, or inside TopicMap the MapTopic entry is
missing.
-}
updateMapTopic : TopicId -> BoxId -> (MapTopic -> MapTopic) -> Model -> Model
updateMapTopic topicId boxId transform model =
  model
    |> updateTopicMap boxId
      (\topicMap ->
        { topicMap | topics = topicMap.topics |> Dict.update (toTopicId topicId)
          (\maybeMapTopic ->
            case maybeMapTopic of
              Just mapTopic__ -> Just (transform mapTopic__)
              Nothing -> Console.logError "TopicMap.TopicMap.updateMapTopic"
                ("Missing MapTopic entry for (" ++ Console.toString topicId ++ ")") Nothing
          )
        }
      )


{-| Canonical TopicMap transformation.
Logs an error if the TopicMap entry is missing.
-}
updateTopicMap : BoxId -> (TopicMap -> TopicMap) -> Model -> Model
updateTopicMap boxId transform ({topicMap} as model) =
  { model | topicMap =
    { topicMap | view = topicMap.view
        |> Dict.update (toBoxId boxId)
          (\maybeTopicMap ->
            case maybeTopicMap of
              Just topicMap_ -> Just (transform topicMap_)
              Nothing -> Console.logError "TopicMap.TopicMap.updateTopicMap"
                ("Missing TopicMap entry for (" ++ Console.toString boxId ++ ")") Nothing
          )
    }
  }


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
  case byId boxId model of
    Just topicMap ->
      Point
        (posAcc.x - topicMap.rect.x1)
        (posAcc.y - topicMap.rect.y1)
    Nothing -> Point 0 0 -- error is already logged
