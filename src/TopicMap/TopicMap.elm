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
import String exposing (fromInt)
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
  Box.topics map.id model
    |> List.map
      (\topic -> itemById_ topic.id map)


assocs : TopicMap -> Model -> List MapItem
assocs map model =
  Box.assocs map.id model
    |> List.map
      (\assoc -> itemById_ assoc.id map)


topicPos : Id -> BoxId -> Model -> Point
topicPos topicId mapId model =
  (topicProps topicId mapId model).pos


setTopicPos : Id -> BoxId -> Point -> Model -> Model
setTopicPos topicId mapId pos model =
  model
    |> updateTopicProps topicId mapId
      (\props -> { props | pos = pos })


updateTopicPos : Id -> BoxId -> (Point -> Point) -> Model -> Model
updateTopicPos topicId mapId transform model =
  model
    |> updateTopicProps topicId mapId
      (\props -> { props | pos = transform props.pos })


{-| Crashes if ID refers not a topic (but an association). -}
topicProps : Id -> BoxId -> Model -> TopicProps
topicProps topicId mapId model =
  case (itemById topicId mapId model).props of
    TopicP props -> props
    AssocP _ -> U.todo
      ("MapItem " ++ fromInt topicId ++ " of TopicMap " ++ fromInt mapId ++
        " is an association when a topic is expected")
      (TopicProps (Point 0 0) Collapsed)


topicPropsOrNothing : Id -> TopicMap -> Maybe TopicProps
topicPropsOrNothing topicId map =
  case map.items |> Dict.get topicId of
    Just mapItem ->
      case mapItem.props of
        TopicP props -> Just props
        AssocP _ -> U.topicMismatch "TopicMap.topicPropsOrNothing" topicId Nothing
    Nothing -> Nothing


assocGeometry : AssocInfo -> BoxId -> Model -> (Point, Point)
assocGeometry assoc mapId model =
  ( topicPos assoc.player1 mapId model
  , topicPos assoc.player2 mapId model
  )


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


initTopicPos : BoxId -> Model -> Point
initTopicPos mapId model =
  let
    map = byId mapId model
  in
  Point
    (C.initTopicPos.x + map.rect.x1 + map.scroll.x)
    (C.initTopicPos.y + map.rect.y1 + map.scroll.y)


itemById : Id -> BoxId -> Model -> MapItem
itemById itemId mapId model =
  byId mapId model
    |> itemById_ itemId


itemById_ : Id -> TopicMap -> MapItem
itemById_ itemId map =
  case map.items |> Dict.get itemId of
    Just mapItem -> mapItem
    Nothing -> U.todo
      ("Missing MapItem " ++ fromInt itemId ++ " in TopicMap " ++ fromInt map.id)
      (MapItem -1 (TopicP (TopicProps (Point 0 0) Collapsed)))


hasItem : Id -> BoxId -> Model -> Bool
hasItem itemId mapId model =
  (byId mapId model).items |> Dict.member itemId


{-| Logs an error if TopicMap does not exist.
### FIXME: support other renderers
-}
fullscreen : Model -> Maybe TopicMap
fullscreen model =
  Just <| byId model.boxId model -- TODO: remove Maybe


byId : BoxId -> Model -> TopicMap
byId mapId model =
  case model.topicMap |> Dict.get mapId of
    Just map -> map
    Nothing -> U.todo
      ("Missing TopicMap " ++ fromInt mapId ++ " in Model.topicMap")
      (TopicMap -1 (Rectangle 0 0 0 0) (Point 0 0) Dict.empty)


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
