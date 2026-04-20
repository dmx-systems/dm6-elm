module TopicMap.TopicMap exposing (update, create, topics, assocs, topicPos, setTopicPos,
  updateTopicPos, topicPropsOrNothing, assocGeometry, addItem, initLimboTopicProps,
  initTopicPos, hasItem, fullscreen, byId, updateRect, updateScrollPos, revelationBoxId,
  revelationBoxPath, landingTarget)

import Box
import Config as C
import Env exposing (Env)
import Feature.SearchDef exposing (SearchResult(..))
import Feature.Sel as Sel
import Item
import Model exposing (Model, Msg)
import ModelBase exposing (..)
import Storage as S
import TopicMap.TopicMapDef as TopicMapDef exposing (TopicMap, MapItem, ItemProps(..),
  TopicProps)
import Undo exposing (UndoModel)
import Utils as U

import Dict
import Random



-- UPDATE


update : TopicMapDef.Msg -> Env -> (UndoModel, Cmd Msg)
update msg ({undoModel} as env) =
  case msg of
    TopicMapDef.AddTopic topicId mapId pos -> addTopic topicId mapId pos env
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
  { model | topicMap = topicMap |> Dict.insert map.id map }


-- TODO: unify these 2
topics : TopicMap -> Model -> List MapItem
topics map model =
  Box.topics map.id model |> List.foldr
    (\topic itemsAcc ->
      case itemById_ topic.id map of
        Just item -> item :: itemsAcc
        Nothing -> itemsAcc
    )
    []


assocs : TopicMap -> Model -> List MapItem
assocs map model =
  Box.assocs map.id model |> List.foldr
    (\assoc itemsAcc ->
      case itemById_ assoc.id map of
        Just item -> item :: itemsAcc
        Nothing -> itemsAcc
    )
    []


{-| Logs an error if TopicMap does not exist, or topic is not in TopicMap, or ID refers not a
topic (but an association).
-}
topicPos : Id -> BoxId -> Model -> Maybe Point
topicPos topicId mapId model =
  case topicProps topicId mapId model of
    Just { pos } -> Just pos
    Nothing -> U.fail "TopicMap.topicPos" {topicId = topicId, mapId = mapId} Nothing


{-| Logs an error if TopicMap does not exist, or if topic is not in TopicMap -}
setTopicPos : Id -> BoxId -> Point -> Model -> Model
setTopicPos topicId mapId pos model =
  model |> updateTopicProps topicId mapId
    (\props -> { props | pos = pos })


{-| Logs an error if TopicMap does not exist, or if topic is not in TopicMap -}
updateTopicPos : Id -> BoxId -> (Point -> Point) -> Model -> Model
updateTopicPos topicId mapId transform model =
  model
    |> updateTopicProps topicId mapId
      (\props -> { props | pos = transform props.pos })


{-| Logs an error if TopicMap does not exist, or topic is not in TopicMap, or ID refers not a
topic (but an association).
-}
topicProps : Id -> BoxId -> Model -> Maybe TopicProps
topicProps topicId mapId model =
  case itemById topicId mapId model of
    Just mapItem ->
      case mapItem.props of
        TopicP props -> Just props
        AssocP _ -> U.topicMismatch "TopicMap.topicProps" topicId Nothing
    Nothing -> U.fail "TopicMap.topicProps" {topicId = topicId, mapId = mapId} Nothing


topicPropsOrNothing : Id -> TopicMap -> Maybe TopicProps
topicPropsOrNothing topicId map =
  case map.items |> Dict.get topicId of
    Just mapItem ->
      case mapItem.props of
        TopicP props -> Just props
        AssocP _ -> U.topicMismatch "TopicMap.topicPropsOrNothing" topicId Nothing
    Nothing -> Nothing


assocGeometry : AssocInfo -> BoxId -> Model -> Maybe (Point, Point)
assocGeometry assoc mapId model =
  let
    pos1 = topicPos assoc.player1 mapId model
    pos2 = topicPos assoc.player2 mapId model
  in
  case Maybe.map2 (\p1 p2 -> (p1, p2)) pos1 pos2 of
    Just geometry -> Just geometry
    Nothing -> U.fail "TopicMap.assocGeometry" { assoc = assoc, mapId = mapId } Nothing


addItem : Id -> BoxId -> PosHint -> Model -> (Model, Cmd Msg)
addItem itemId mapId posHint model =
  if hasItem itemId mapId model then
    (model, Cmd.none)
  else
    if posHint == Random then
      let
        toMsg = Model.TopicMap << TopicMapDef.AddTopic itemId mapId
      in
      (model, Random.generate toMsg pointGen)
    else
      let
        mapItem = MapItem itemId <| initItemProps itemId mapId model
        _ = U.info "TopicMap.addItem" {itemId = itemId, mapId = mapId}
      in
      ( model |> updateTopicMap mapId
          (\map ->
            { map | items = map.items |> Dict.insert itemId mapItem }
          )
      , Cmd.none
      )


addTopic : Id -> BoxId -> Point -> Env -> Model
addTopic topicId mapId pos ({model} as env) =
  model
    |> updateTopicMap mapId (addTopic_ topicId pos)
    |> Env.autoSize env


addTopic_ : Id -> Point -> TopicMap -> TopicMap
addTopic_ topicId pos map =
  { map | items = map.items |> Dict.insert topicId
      (MapItem topicId <| TopicP <| TopicProps pos Collapsed)
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


{-| Initial props for a revealed item -}
initItemProps : Id -> BoxId -> Model -> ItemProps
initItemProps itemId mapId model =
  case Item.byId itemId model of
    Just item ->
      case item.info of
        Topic _ -> TopicP <| initTopicProps itemId mapId model
        Assoc _ -> AssocP {}
    Nothing -> AssocP {} -- error is already logged


{-| Initial props for a revealed topic -}
initTopicProps : Id -> BoxId -> Model -> TopicProps
initTopicProps topicId mapId model =
  TopicProps
    (initTopicPos mapId model)
    Collapsed


initLimboTopicProps : Id -> BoxId -> Model -> TopicProps
initLimboTopicProps topicId mapId model =
  TopicProps
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


{-| Logs an error if box does not exist, or item is not in box. -}
itemById : Id -> BoxId -> Model -> Maybe MapItem
itemById itemId mapId model =
  byId mapId model
    |> Maybe.andThen (itemById_ itemId)


itemById_ : Id -> TopicMap -> Maybe MapItem
itemById_ itemId map =
  case map.items |> Dict.get itemId of
    Just mapItem -> Just mapItem
    Nothing -> U.itemNotInBox "TopicMap.itemById_" itemId map.id Nothing


hasItem : Id -> BoxId -> Model -> Bool
hasItem itemId mapId model =
  case byId mapId model of
    Just map -> map.items |> Dict.member itemId
    Nothing -> U.fail "TopicMap.hasItem" {itemId = itemId, mapId = mapId} False


{-| Logs an error if TopicMap does not exist.
### FIXME: support other renderers
-}
fullscreen : Model -> Maybe TopicMap
fullscreen model =
  byId model.boxId model


{-| Logs an error if TopicMap does not exist. -}
byId : BoxId -> Model -> Maybe TopicMap
byId mapId model =
  case model.topicMap |> Dict.get mapId of
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


{-| Canonical TopicProps transformation.
Logs an error if box does not exist, or topic is not in box, or ID refers not a topic (but
an association).
-}
updateTopicProps : Id -> BoxId -> (TopicProps -> TopicProps) -> Model -> Model
updateTopicProps topicId mapId transform model =
  model |> updateTopicMap mapId
    (\map ->
      { map | items = map.items |> Dict.update topicId
        (\item_ ->
          case item_ of
            Just item ->
              case item.props of
                TopicP props -> Just
                  { item | props = TopicP (transform props) }
                AssocP _ -> U.topicMismatch "TopicMap.updateTopicProps" topicId Nothing
            Nothing -> U.itemNotFound "TopicMap.updateTopicProps" topicId Nothing
        )
      }
    )


{-| Canonical TopicMap transformation.
Logs an error if TopicMap does not exist.
-}
updateTopicMap : BoxId -> (TopicMap -> TopicMap) -> Model -> Model
updateTopicMap mapId transform ({topicMap} as model) =
  { model | topicMap =
    topicMap |> Dict.update mapId
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
    boxId :: boxPath -> Just (boxId, boxPath)
