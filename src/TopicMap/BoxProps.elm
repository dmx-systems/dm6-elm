module TopicMap.BoxProps exposing (init, create, allTopicProps, topicPos, setTopicPos,
  updateTopicPos, topicPropsOrNothing, assocGeometry, addTopic, addTopic_, initLimboTopicProps,
  initTopicPos, hasTopicProps, fullscreen, byId, updateRect, updateScrollPos, revelationBoxId,
  revelationBoxPath, landingTarget)

import Box
import Config as C
import Env exposing (Env, Env2)
import Feature.SearchDef exposing (SearchResult(..))
import Feature.Sel as Sel
import Model exposing (Model, Msg)
import ModelBase exposing (..)
import TopicMap.TopicMapDef as TopicMapDef exposing (BoxProps, TopicProps)
import Utils as U

import Dict
import Random
import String exposing (fromInt)



init : BoxId -> Model -> Model
init boxId model =
  model -- TODO


create : BoxId -> Model -> Model
create boxId model =
  let
    boxProps = BoxProps boxId (Rectangle 0 0 0 0) (Point 0 0) Dict.empty
  in
  model
    |> create_ boxProps


create_ : BoxProps -> Model -> Model
create_ boxProps ({topicMap} as model) =
  { model | topicMap =
    { topicMap | boxProps = topicMap.boxProps |> Dict.insert (toBoxId boxProps.id) boxProps }
  }


{-| Projects the given BoxProps into what is to be rendered based on actual box content.
Note: the box content is the source of truth. BoxProps on the other hand remember everything
once rendered. ### TODO: function name
-}
allTopicProps : BoxProps -> Model -> List TopicProps
allTopicProps boxProps model =
  Box.topicIds boxProps.id model
    |> List.filterMap
      (\topicId -> topicProps_ topicId boxProps)


{-| Logs an error if the BoxProps entry is missing, or inside BoxProps the TopicProps entry is
missing.
-}
topicPos : TopicId -> BoxId -> Model -> Maybe Point
topicPos topicId boxId model =
  case topicProps topicId boxId model of
    Just {pos} -> Just pos
    Nothing -> U.fail "TopicMap.BoxProps.topicPos" {topicId = topicId, boxId = boxId} Nothing


{-| Logs an error if the BoxProps entry is missing, or inside BoxProps the TopicProps entry is
missing. -}
setTopicPos : TopicId -> BoxId -> Point -> Model -> Model
setTopicPos topicId boxId pos model =
  model
    |> updateTopicPos topicId boxId (\_ -> pos)


{-| Logs an error if the BoxProps entry is missing, or inside BoxProps the TopicProps entry is
missing. -}
updateTopicPos : TopicId -> BoxId -> (Point -> Point) -> Model -> Model
updateTopicPos topicId boxId transform model =
  model
    |> updateTopicProps topicId boxId
      (\topic -> { topic | pos = transform topic.pos })


{-| Canonical TopicProps transformation.
Logs an error if the BoxProps entry is missing, or inside BoxProps the TopicProps entry is
missing.
-}
updateTopicProps : TopicId -> BoxId -> (TopicProps -> TopicProps) -> Model -> Model
updateTopicProps topicId boxId transform model =
  model
    |> updateBoxProps boxId
      (\boxProps ->
        { boxProps | topicProps = boxProps.topicProps |> Dict.update (toTopicId topicId)
          (\maybeTopicProps ->
            case maybeTopicProps of
              Just tProps -> Just (transform tProps)
              Nothing -> U.logError "TopicMap.BoxProps.updateTopicProps"
                ("Missing TopicProps for " ++ U.toString topicId) Nothing
          )
        }
      )


assocGeometry : Assoc -> BoxId -> Model -> Maybe (Point, Point)
assocGeometry assoc boxId model =
  let
    pos1 = topicPos assoc.topicId1 boxId model
    pos2 = topicPos assoc.topicId2 boxId model
  in
  case Maybe.map2 (\p1 p2 -> (p1, p2)) pos1 pos2 of
    Just geometry -> Just geometry
    Nothing -> U.fail "TopicMap.BoxProps.assocGeometry" {assoc = assoc, boxId = boxId} Nothing


addTopic : TopicId -> BoxId -> PosHint -> Env2 -> (Model, Cmd Msg)
addTopic topicId boxId posHint {model} =
  if hasTopicProps topicId boxId model then
    (model, Cmd.none)
  else
    if posHint == Random then
      let
        toMsg = Model.TopicMap << TopicMapDef.GotRandomPos topicId boxId
      in
      (model, Random.generate toMsg pointGen)
    else
      let
        topic = initTopicProps topicId boxId model
        _ = U.info "TopicMap.BoxProps.addTopic" {topicId = topicId, boxId = boxId}
      in
      ( model |> updateBoxProps boxId
          (\boxProps ->
            { boxProps | topicProps =
                boxProps.topicProps |> Dict.insert (toTopicId topicId) topic
            }
          )
      , Cmd.none
      )


addTopic_ : TopicId -> BoxId -> Point -> Env -> Model
addTopic_ topicId boxId pos ({model} as env) =
  model
    |> updateBoxProps boxId (addTopic__ topicId pos)
    |> Env.autoSize env


addTopic__ : TopicId -> Point -> BoxProps -> BoxProps
addTopic__ topicId pos boxProps =
  { boxProps | topicProps = boxProps.topicProps |> Dict.insert (toTopicId topicId)
      (TopicProps topicId pos Collapsed)
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
initTopicProps : TopicId -> BoxId -> Model -> TopicProps
initTopicProps topicId boxId model =
  TopicProps
    topicId
    (initTopicPos boxId model)
    Collapsed


initLimboTopicProps : TopicId -> BoxId -> Model -> TopicProps
initLimboTopicProps topicId boxId model =
  TopicProps
    topicId
    (initTopicPos boxId model)
    Expanded


{-| Logs an error if the BoxProps entry is missing. -}
initTopicPos : BoxId -> Model -> Point
initTopicPos boxId model =
  case byId boxId model of
    Just boxProps ->
      Point
        (C.initTopicPos.x + boxProps.rect.x1 + boxProps.scroll.x)
        (C.initTopicPos.y + boxProps.rect.y1 + boxProps.scroll.y)
    Nothing -> Point 0 0 -- error is already logged


{-| Looks up TopicProps for a given BoxId.
Logs an error if the BoxProps entry is missing, or inside BoxProps the TopicProps entry is
missing.
-}
topicProps : TopicId -> BoxId -> Model -> Maybe TopicProps
topicProps topicId boxId model =
  byId boxId model
    |> Maybe.andThen (topicProps_ topicId)


{-| Looks up TopicProps inside the given BoxProps.
Logs an error if the TopicProps entry is missing.
-}
topicProps_ : TopicId -> BoxProps -> Maybe TopicProps
topicProps_ topicId boxProps =
  case topicPropsOrNothing topicId boxProps of
    Just topic -> Just topic
    Nothing -> U.logError "TopicMap.BoxProps.topicProps_"
      ("Missing TopicProps " ++ U.toString topicId ++ " inside " ++ U.toString boxProps.id)
      Nothing


topicPropsOrNothing : TopicId -> BoxProps -> Maybe TopicProps
topicPropsOrNothing topicId boxProps =
  case boxProps.topicProps |> Dict.get (toTopicId topicId) of
    Just topic -> Just topic
    Nothing -> Nothing


hasTopicProps : TopicId -> BoxId -> Model -> Bool
hasTopicProps topicId boxId model =
  case byId boxId model of
    Just boxProps -> boxProps.topicProps |> Dict.member (toTopicId topicId)
    Nothing -> U.fail "TopicMap.BoxProps.hasTopicProps" {topicId = topicId, boxId = boxId} False


{-| Logs an error if the BoxProps entry is missing.
### FIXME: make it an extension point
-}
fullscreen : Model -> Maybe BoxProps
fullscreen model =
  byId model.boxId model


{-| Logs an error if the BoxProps entry is missing.
-}
byId : BoxId -> Model -> Maybe BoxProps
byId boxId model =
  case model.topicMap.boxProps |> Dict.get (toBoxId boxId) of
    Just boxProps -> Just boxProps
    Nothing -> U.logError "TopicMap.BoxProps.byId" ("Missing BoxProps for " ++ U.toString boxId)
      Nothing


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


{-| Canonical BoxProps transformation.
Logs an error if the BoxProps entry is missing.
-}
updateBoxProps : BoxId -> (BoxProps -> BoxProps) -> Model -> Model
updateBoxProps boxId transform ({topicMap} as model) =
  { model | topicMap =
    { topicMap | boxProps = topicMap.boxProps |> Dict.update (toBoxId boxId)
      (\maybeBoxProps ->
        case maybeBoxProps of
          Just boxProps -> Just (transform boxProps)
          Nothing -> U.logError "TopicMap.BoxProps.updateBoxProps"
            ("Missing BoxProps for " ++ U.toString boxId) Nothing
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
