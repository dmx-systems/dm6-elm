module TopicMap.TopicMap exposing (init, create, allTopicProps, topicPos, setTopicRandomPos,
  setTopicPos, updateTopicPos, topicPropsOrNothing, assocGeometry, addTopicAt,
  initLimboTopicProps, initTopicPos, hasTopicProps, fullscreen, byId, updateRect,
  updateScrollPos, revelationBoxId, revelationBoxPath, landingTarget)

import Box
import Config as C
import Env exposing (Env)
import Feature.SearchDef exposing (SearchResult(..))
import Feature.Sel as Sel
import Model exposing (Model, Msg)
import ModelBase exposing (..)
import Topic
import TopicMap.TopicMapDef as TopicMapDef exposing (TopicMap, MapTopic)
import Utils as U

import Dict
import Random



-- ExtManager.ExtInit
init : BoxId -> Model -> Model
init boxId model =
  model
    |> Box.topicIds boxId
    |> List.foldl
      (\topicId acc ->
        if Topic.isBox topicId acc then
          init (BoxId topicId) acc -- recursion
        else
          acc
      )
      model
    |> createBoxProps boxId
    |> addMissingTopicProps boxId


createBoxProps : BoxId -> Model -> Model
createBoxProps boxId model =
  if Dict.member (toBoxId boxId) model.topicMap.maps then
    let
      _ = U.info "TopicMap.TopicMap.createBoxProps"
        ("Box (" ++ U.toString boxId ++ ") has TopicMap entry already")
    in
    model
  else
    let
      _ = U.info "TopicMap.TopicMap.createBoxProps"
        ("Creating TopicMap entry for box (" ++ U.toString boxId ++ ")")
    in
    create boxId model


addMissingTopicProps : BoxId -> Model -> Model
addMissingTopicProps boxId model =
  model
    |> updateBoxProps boxId
      (\boxProps ->
        { boxProps | topics =
            Box.topicIds boxId model
              |> List.foldl
                (\topicId acc ->
                  let
                    id = toTopicId topicId
                  in
                  if Dict.member id acc then
                    acc
                  else
                    acc |> Dict.insert id (initTopicProps topicId boxId model)
                )
                boxProps.topics
        }
      )


create : BoxId -> Model -> Model
create boxId model =
  let
    boxProps = TopicMap boxId (Rectangle 0 0 0 0) (Point 0 0) Dict.empty
  in
  model
    |> create_ boxProps


create_ : TopicMap -> Model -> Model
create_ boxProps ({topicMap} as model) =
  { model | topicMap =
    { topicMap | maps = topicMap.maps
        |> Dict.insert (toBoxId boxProps.id) boxProps
    }
  }


--

{-| Projects the given TopicMap into what is to be rendered based on actual box content.
Note: the box content is the source of truth. TopicMap on the other hand remember everything
once rendered. ### TODO: function name
-}
allTopicProps : TopicMap -> Model -> List MapTopic
allTopicProps boxProps model =
  Box.topicIds boxProps.id model
    |> List.filterMap
      (\topicId -> topicProps_ topicId boxProps)


{-| Logs an error if the TopicMap entry is missing, or inside TopicMap the MapTopic entry is
missing.
-}
topicPos : TopicId -> BoxId -> Model -> Maybe Point
topicPos topicId boxId model =
  case topicProps topicId boxId model of
    Just {pos} -> Just pos
    Nothing -> U.fail "TopicMap.TopicMap.topicPos" {topicId = topicId, boxId = boxId} Nothing


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
    |> updateTopicProps topicId boxId
      (\topic -> { topic | pos = transform topic.pos })


assocGeometry : Assoc -> BoxId -> Model -> Maybe (Point, Point)
assocGeometry assoc boxId model =
  let
    pos1 = topicPos assoc.topicId1 boxId model
    pos2 = topicPos assoc.topicId2 boxId model
  in
  case Maybe.map2 (\p1 p2 -> (p1, p2)) pos1 pos2 of
    Just geometry -> Just geometry
    Nothing -> U.fail "TopicMap.TopicMap.assocGeometry" {assoc = assoc, boxId = boxId} Nothing


setTopicRandomPos : TopicId -> BoxId -> Model -> (Model, Cmd Msg)
setTopicRandomPos topicId boxId model =
  model
    |> init boxId
    |> randomPos topicId boxId


randomPos : TopicId -> BoxId -> Model -> (Model, Cmd Msg)
randomPos topicId boxId model =
  let
    toMsg = Model.TopicMap << TopicMapDef.GotRandomPos topicId boxId
    cmd = Random.generate toMsg pointGen
  in
  (model, cmd)


-- GotRandomPos message handler
addTopicAt : TopicId -> BoxId -> Point -> Env -> Model
addTopicAt topicId boxId pos ({model} as env) =
  model
    |> updateBoxProps boxId (addTopic_ topicId pos)
    |> Env.autoSize env


addTopic_ : TopicId -> Point -> TopicMap -> TopicMap
addTopic_ topicId pos boxProps =
  { boxProps | topics =
      boxProps.topics |> Dict.insert
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


{-| Initial props for a revealed topic -}
initTopicProps : TopicId -> BoxId -> Model -> MapTopic
initTopicProps topicId boxId model =
  MapTopic
    topicId
    (initTopicPos boxId model)
    Collapsed


initLimboTopicProps : TopicId -> BoxId -> Model -> MapTopic
initLimboTopicProps topicId boxId model =
  MapTopic
    topicId
    (initTopicPos boxId model)
    Expanded


{-| Logs an error if the TopicMap entry is missing. -}
initTopicPos : BoxId -> Model -> Point
initTopicPos boxId model =
  case byId boxId model of
    Just boxProps ->
      Point
        (C.initTopicPos.x + boxProps.rect.x1 + boxProps.scroll.x)
        (C.initTopicPos.y + boxProps.rect.y1 + boxProps.scroll.y)
    Nothing -> Point 0 0 -- error is already logged


{-| Looks up MapTopic for a given BoxId.
Logs an error if the TopicMap entry is missing, or inside TopicMap the MapTopic entry is
missing.
-}
topicProps : TopicId -> BoxId -> Model -> Maybe MapTopic
topicProps topicId boxId model =
  byId boxId model
    |> Maybe.andThen (topicProps_ topicId)


{-| Looks up MapTopic inside the given TopicMap.
Logs an error if the MapTopic entry is missing.
-}
topicProps_ : TopicId -> TopicMap -> Maybe MapTopic
topicProps_ topicId boxProps =
  case topicPropsOrNothing topicId boxProps of
    Just topic -> Just topic
    Nothing -> U.logError "TopicMap.TopicMap.topicProps_" ("Missing MapTopic ("
      ++ U.toString topicId ++ ") inside (" ++ U.toString boxProps.id ++ ")") Nothing


topicPropsOrNothing : TopicId -> TopicMap -> Maybe MapTopic
topicPropsOrNothing topicId boxProps =
  case boxProps.topics |> Dict.get (toTopicId topicId) of
    Just topic -> Just topic
    Nothing -> Nothing


hasTopicProps : TopicId -> BoxId -> Model -> Bool
hasTopicProps topicId boxId model =
  case byId boxId model of
    Just boxProps -> boxProps.topics |> Dict.member (toTopicId topicId)
    Nothing -> U.fail "TopicMap.TopicMap.hasTopicProps" {topicId = topicId, boxId = boxId} False


{-| Logs an error if the TopicMap entry is missing.
### FIXME: make it an extension point
-}
fullscreen : Model -> Maybe TopicMap
fullscreen model =
  byId model.boxId model


{-| Logs an error if the TopicMap entry is missing.
-}
byId : BoxId -> Model -> Maybe TopicMap
byId boxId model =
  case model.topicMap.maps |> Dict.get (toBoxId boxId) of
    Just boxProps -> Just boxProps
    Nothing -> U.logError "TopicMap.TopicMap.byId"
      ("Missing TopicMap entry for (" ++ U.toString boxId ++ ")") Nothing


--

updateRect : BoxId -> (Rectangle -> Rectangle) -> Model -> Model
updateRect boxId transform model =
  model
    |> updateBoxProps boxId
      (\boxProps ->
        { boxProps | rect = transform boxProps.rect }
      )


updateScrollPos : BoxId -> (Point -> Point) -> Model -> Model
updateScrollPos boxId transform model =
  model
    |> updateBoxProps boxId
      (\boxProps ->
        { boxProps | scroll = transform boxProps.scroll }
      )


{-| Canonical MapTopic transformation.
Logs an error if the TopicMap entry is missing, or inside TopicMap the MapTopic entry is
missing.
-}
updateTopicProps : TopicId -> BoxId -> (MapTopic -> MapTopic) -> Model -> Model
updateTopicProps topicId boxId transform model =
  model
    |> updateBoxProps boxId
      (\boxProps ->
        { boxProps | topics = boxProps.topics |> Dict.update (toTopicId topicId)
          (\maybeTopicProps ->
            case maybeTopicProps of
              Just tProps -> Just (transform tProps)
              Nothing -> U.logError "TopicMap.TopicMap.updateTopicProps"
                ("Missing MapTopic entry for (" ++ U.toString topicId ++ ")") Nothing
          )
        }
      )


{-| Canonical TopicMap transformation.
Logs an error if the TopicMap entry is missing.
-}
updateBoxProps : BoxId -> (TopicMap -> TopicMap) -> Model -> Model
updateBoxProps boxId transform ({topicMap} as model) =
  { model | topicMap =
    { topicMap | maps = topicMap.maps
        |> Dict.update (toBoxId boxId)
          (\maybeBoxProps ->
            case maybeBoxProps of
              Just boxProps -> Just (transform boxProps)
              Nothing -> U.logError "TopicMap.TopicMap.updateBoxProps"
                ("Missing TopicMap entry for (" ++ U.toString boxId ++ ")") Nothing
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
