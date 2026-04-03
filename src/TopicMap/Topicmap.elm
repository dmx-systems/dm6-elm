module TopicMap.TopicMap exposing (fullscreen, byId, byIdOrLog, update, updateRect,
  updateScrollPos, visibleTopics, topicPos, setTopicPos, updateTopicPos, topicProps,
  initItemProps, initTopicProps, initLimboTopicProps, initTopicPos, assocGeometry, create,
  addItem, showItem, removeItem, removeItem_, revelationBoxId, revelationBoxPath, landingTarget,
  isEmpty, isUnboxed, isTopic, isAssoc, isVisible, isPinned)

import Box
import Config as C
import Feature.Search exposing (SearchResult(..))
import Feature.SelAPI as SelAPI
import Item
import Model exposing (Model)
import ModelParts exposing (..)
import TopicMap.TopicMapDef exposing (TopicMap, MapItems, MapItem, Visibility(..), Pinned(..),
  ItemProps(..), TopicProps)
import Utils as U

import Dict
import String exposing (fromInt)



fullscreen : Model -> Maybe TopicMap
fullscreen model =
  byIdOrLog model.boxId model


{-| Logs an error if TopicMap does not exist. -}
byIdOrLog : BoxId -> Model -> Maybe TopicMap
byIdOrLog mapId model =
  case byId mapId model of
    Just map -> Just map
    Nothing -> U.illegalBoxId "byIdOrLog" mapId Nothing


byId : BoxId -> Model -> Maybe TopicMap
byId mapId model =
  model.topicMap
    |> Dict.get mapId


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


updateRect : BoxId -> (Rectangle -> Rectangle) -> Model -> Model
updateRect mapId transform model =
  model |> update mapId
    (\map ->
      { map | rect = transform map.rect }
    )


updateScrollPos : BoxId -> (Point -> Point) -> Model -> Model
updateScrollPos mapId transform model =
  model |> update mapId
    (\map ->
      { map | scroll = transform map.scroll }
    )


visibleTopics : TopicMap -> List MapItem
visibleTopics map =
  map.items |> Dict.values |> List.filter isTopic |> List.filter isVisible


{-| Logs an error if TopicMap does not exist, or topic is not in TopicMap, or ID refers not a
topic (but an association).
-}
topicPos : Id -> BoxId -> Model -> Maybe Point
topicPos topicId mapId model =
  case topicProps topicId mapId model of
    Just { pos } -> Just pos
    Nothing -> U.fail "topicPos" {topicId = topicId, mapId = mapId} Nothing


{-| Logs an error if TopicMap does not exist, or if topic is not in TopicMap -}
setTopicPos : Id -> BoxId -> Point -> Model -> Model
setTopicPos topicId mapId pos model =
  model |> updateTopicProps_ topicId mapId
    (\props -> { props | pos = pos })


{-| Logs an error if TopicMap does not exist, or if topic is not in TopicMap -}
updateTopicPos : Id -> BoxId -> (Point -> Point) -> Model -> Model
updateTopicPos topicId mapId transform model =
  model
    |> updateTopicProps_ topicId mapId
      (\props -> { props | pos = transform props.pos })


{-| Logs an error if TopicMap does not exist, or topic is not in TopicMap, or ID refers not a
topic (but an association).
-}
topicProps : Id -> BoxId -> Model -> Maybe TopicProps
topicProps topicId mapId model =
  case itemByIdOrLog_ topicId mapId model of
    Just mapItem ->
      case mapItem.props of
        TopicP props -> Just props
        AssocP _ -> U.topicMismatch "topicProps" topicId Nothing
    Nothing -> U.fail "topicProps" {topicId = topicId, mapId = mapId} Nothing


assocGeometry : AssocInfo -> BoxId -> Model -> Maybe (Point, Point)
assocGeometry assoc mapId model =
  let
    pos1 = topicPos assoc.player1 mapId model
    pos2 = topicPos assoc.player2 mapId model
  in
  case Maybe.map2 (\p1 p2 -> (p1, p2)) pos1 pos2 of
    Just geometry -> Just geometry
    Nothing -> U.fail "assocGeometry" { assoc = assoc, mapId = mapId } Nothing


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
    ( initTopicPos mapId model )
    ( case Item.isBox topicId model of
        True -> BoxD BlackBox
        False -> TopicD LabelOnly
    )


initLimboTopicProps : Id -> BoxId -> Model -> TopicProps
initLimboTopicProps topicId mapId model =
  TopicProps
    ( initTopicPos mapId model )
    ( case Item.isBox topicId model of
        True -> BoxD WhiteBox
        False -> TopicD Detail
    )

{-| Logs an error if box does not exist. -}
initTopicPos : BoxId -> Model -> Point
initTopicPos mapId model =
  case byIdOrLog mapId model of
    Just map ->
      Point
        (C.initTopicPos.x + map.rect.x1 + map.scroll.x)
        (C.initTopicPos.y + map.rect.y1 + map.scroll.y)
    Nothing -> Point 0 0 -- error is already logged


{-| Logs an error if box does not exist, or item is not in box. -}
itemByIdOrLog_ : Id -> BoxId -> Model -> Maybe MapItem
itemByIdOrLog_ itemId mapId model =
  byIdOrLog mapId model |> Maybe.andThen
    (\map ->
      case map.items |> Dict.get itemId of
        Just mapItem -> Just mapItem
        Nothing -> U.itemNotInBox "itemByIdOrLog_" itemId map.id Nothing
    )


{-| Adds an item to a box and creates a connecting association.
Presumption: the item is not yet contained in the box. Otherwise the existing box-item would be
overridden and another association still be created. This is not what you want.
It's a generic operation: works for both, topics and associations.
-}
addItem : Id -> ItemProps -> BoxId -> Model -> Model
addItem itemId props mapId model =
  let
    mapItem = MapItem itemId (Visible Unpinned) props
    _ = U.info "addItem" { itemId = itemId, props = props, mapId = mapId}
  in
  model
    |> update mapId
      (\map -> { map | items = map.items |> Dict.insert itemId mapItem })


{-| Sets the item's "visibility" field to Visible (Pinned=False).
Presumption: the item *is* contained in the box.
No-op if the item is *not* contained in the box, or its "visibility" field is Visible already.
Logs an error if box does not exist.
It's a generic operation: works for both, topics and associations.
-}
showItem : Id -> BoxId -> Model -> Model
showItem itemId mapId model =
  model |> update mapId
    (\map ->
      { map | items = map.items |> Dict.update itemId
        (\maybeItem ->
          case maybeItem of
            Just mapItem -> Just
              { mapItem | visibility =
                case mapItem.visibility of
                  Visible _ -> mapItem.visibility
                  Removed -> Visible Unpinned
              }
            Nothing -> Nothing
        )
      }
    )


{-| Removes an item from a box, along its associations in that box context.
No-op if the item is *not* contained in the box.
Logs an error if box does not exist.
It's a generic operation: works for both, topics and associations.
Note: while this functions supports associations as players in associations,
at the moment DM6 Elm makes no use of it.
-}
removeItem : Id -> BoxId -> Model -> Model
removeItem itemId mapId model =
  model
    |> update mapId
        (\map ->
          { map | items = removeItem_ itemId map.items model }
        )


{-| Removes an item from a box, along its associations in that box context.
No-op if the item is *not* contained in the box.
Convenience API to operate on given MapItems directly
-}
removeItem_ : Id -> MapItems -> Model -> MapItems
removeItem_ itemId items model =
  assocsOfPlayer_ itemId items model |> List.foldr
    (\assocId itemsAcc -> removeItem_ assocId itemsAcc model) -- recursion
    (removeItem__ itemId items)


{-| Removes an item (sets its visibility to Removed) from a set of MapItems.
No-op if the item is *not* contained in the set of MapItems.
Low-level API that does NOT remove the item's associations.
-}
removeItem__ : Id -> MapItems -> MapItems
removeItem__ itemId items =
  items |> Dict.update itemId
    (\maybeItem ->
      case maybeItem of
        Just item -> Just { item | visibility = Removed }
        Nothing -> Nothing
    )


{-| Canonical TopicMap transformation.
Logs an error if TopicMap does not exist.
-}
update : BoxId -> (TopicMap -> TopicMap) -> Model -> Model
update mapId transform ({topicMap} as model) =
  { model | topicMap =
    topicMap |> Dict.update mapId
      (\maybeMap ->
        case maybeMap of
          Just map -> Just (transform map)
          Nothing -> U.illegalBoxId "update" mapId Nothing
      )
  }


{-| Returns a player's associations (their Ids) in the given box context (MapItems).
Low-level API to operate on given MapItems directly.
-}
assocsOfPlayer_ : Id -> MapItems -> Model -> List Id
assocsOfPlayer_ playerId items model =
  items
    |> Dict.values
    |> List.filter isAssoc
    |> List.map .id
    |> List.filter (Item.hasPlayer playerId model)


--

{-| Canonical TopicProps transformation.
Logs an error if box does not exist, or topic is not in box, or ID refers not a topic (but
an association).
-}
updateTopicProps_ : Id -> BoxId -> (TopicProps -> TopicProps) -> Model -> Model
updateTopicProps_ topicId mapId transform model =
  model |> update mapId
    (\map ->
      { map | items = map.items |> Dict.update topicId
        (\item_ ->
          case item_ of
            Just item ->
              case item.props of
                TopicP props -> Just
                  { item | props = TopicP (transform props) }
                AssocP _ -> U.topicMismatch "updateTopicProps_" topicId Nothing
            Nothing -> U.illegalItemId "updateTopicProps_" topicId Nothing
        )
      }
    )


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
      Just <| SelAPI.landingBoxPath model
    RelTopics _ _ ->
      case SelAPI.single model of
        Just (_, boxPath) -> Just boxPath
        Nothing -> Nothing
    NoSearch -> Nothing


{- The landing box as a selection target. For a fullscreen box Nothing is returned. -}
landingTarget : Model -> Maybe (BoxId, BoxPath)
landingTarget model =
  case SelAPI.landingBoxPath model of
    [] -> Nothing
    [ boxId ] -> Nothing -- The fullscreen box is never selected
    boxId :: boxPath -> Just (boxId, boxPath)


{-| Logs an error if box does not exist. -}
isEmpty : BoxId -> Model -> Bool
isEmpty boxId model =
  case byIdOrLog boxId model of
    Just map -> map.items |> Dict.values |> List.any isVisible |> not
    Nothing -> False


{-| Logs an error if box does not exist, or topic is not in box, or ID refers not a topic (but
an association).
-}
isUnboxed : Id -> BoxId -> Model -> Bool
isUnboxed topicId boxId model =
  Box.displayMode topicId boxId model == Just (BoxD Unboxed)


{-| useful as a filter predicate -}
isTopic : MapItem -> Bool
isTopic item =
  case item.props of
    TopicP _ -> True
    AssocP _ -> False


{-| useful as a filter predicate -}
isAssoc : MapItem -> Bool
isAssoc =
  not << isTopic


{-| useful as a filter predicate -}
isVisible : MapItem -> Bool
isVisible item =
  case item.visibility of
    Visible _ -> True
    Removed -> False


isPinned : MapItem -> Bool
isPinned item =
  item.visibility == Visible Pinned
