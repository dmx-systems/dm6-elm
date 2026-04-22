module TopicMap.Geometry exposing (hitTest, toolbarPos)

import Box
import Config as C
import Env exposing (ExtManager)
import Item
import Model exposing (Model)
import ModelBase exposing (..)
import TopicMap.TopicMap as TM
import TopicMap.TopicMapDef exposing (TopicMap, MapItem)
import Utils as U



-- HIT TEST


type alias MapItems =
  List MapItem


{-| Finds the topic/box at a given screen position.
Returns the found topic/box (Id) and its context (BoxPath), or Nothing.
If `excludeTopicId` is given that topic/box will be excluded from search.
-}
hitTest : BoxId -> BoxPath -> Point -> Maybe Id -> ExtManager -> Model -> Maybe Target
hitTest boxId boxPath pos excludeTopicId ext model =
  let
    map = TM.byId boxId model
    items = TM.topics map model
    relPos = mapOffset pos map
  in
  case testChildren relPos items (boxId :: boxPath) excludeTopicId ext model of
    Just target -> Just target
    Nothing ->
      if Box.isFullscreen boxId model then
        Nothing
      else
        if isBoxRectHit pos map.rect then
          Just (boxId, boxPath)
        else
          Nothing


testChildren : Point -> MapItems -> BoxPath -> Maybe Id -> ExtManager -> Model -> Maybe Target
testChildren pos items boxPath excludeTopicId ext model =
  case items of
    [] -> Nothing
    item :: tailItems ->
      let
        maybeItem : Bool -> Maybe Target
        maybeItem found = if found then Just (item.id, boxPath) else Nothing
        --
        boxId = Box.firstId boxPath
        isHeaderHit = isTopicHeaderHit pos item.id boxId >> maybeItem
        relPos = relPos_ pos item.id boxPath
        maybeTarget =
          case (Item.isBox item.id model, Box.expansionOf item.id boxId model) of
            (True, Collapsed) -> isHeaderHit model
            (True, Expanded) ->
              case ext.hitTest item.id boxPath (relPos model) excludeTopicId model of
                Just target -> Just target
                Nothing -> isHeaderHit model
            (False, _) -> isTopicHit item.id boxPath pos model |> maybeItem
        -- recursion
        testTailItems = testChildren pos tailItems boxPath excludeTopicId ext
      in
      -- return item if successfully tested AND not excluded by filter
      case (maybeTarget, excludeTopicId) of
        (Just ((targetId, _) as target), Just topicId) ->
          case targetId /= topicId of
            True -> Just target
            False -> testTailItems model
        (Just target, Nothing) -> Just target
        (Nothing, _) -> testTailItems model


isTopicHit : Id -> BoxPath -> Point -> Model -> Bool
isTopicHit itemId boxPath pos model =
  let
    boxId = Box.firstId boxPath -- Note: a topic is never displayed fullscreen
    isHeaderHit = isTopicHeaderHit pos itemId boxId
    isDetailHit = isTopicDetailHit pos itemId boxId
  in
  -- test depends on topic's expansion
  case Box.expansionOf itemId boxId model of
    Collapsed -> isHeaderHit model
    Expanded -> isHeaderHit model || isDetailHit model


{-| Transforms the given screen position to a map-relative position according to the given map.
-}
mapOffset : Point -> TopicMap -> Point
mapOffset pos map =
  Point
    (pos.x + map.rect.x1 + map.scroll.x)
    (pos.y + map.rect.y1 + map.scroll.y)


relPos_ : Point -> BoxId -> BoxPath -> Model -> Point
relPos_ pos boxId boxPath model =
  let
    boxPos = TM.topicPos boxId (Box.firstId boxPath) model
  in
  Point
    (pos.x - boxPos.x + C.topicW2)
    (pos.y - boxPos.y - C.topicH2)


-- TODO: factor out common TopicMap.Size code

isTopicHeaderHit : Point -> Id -> BoxId -> Model -> Bool
isTopicHeaderHit pos topicId boxId model =
  let
    topicPos = TM.topicPos topicId boxId model
  in
  pos.x > topicPos.x - C.topicW2 - C.topicHeight && -- left edge includes caret area
  pos.x < topicPos.x + C.topicW2 &&
  pos.y > topicPos.y - C.topicH2 &&
  pos.y < topicPos.y + C.topicH2


isTopicDetailHit : Point -> Id -> BoxId -> Model -> Bool
isTopicDetailHit pos topicId boxId model =
  let
    topicPos = TM.topicPos topicId boxId model
  in
  case Item.topicSize topicId .view model of
    Just size ->
      pos.x > topicPos.x - C.topicW2 + C.topicHeight && -- topicHeight = icon box width
      pos.x < topicPos.x - C.topicW2 + C.topicHeight + size.w &&
      pos.y > topicPos.y - C.topicH2 &&
      pos.y < topicPos.y - C.topicH2 + size.h
    _ -> False


isBoxRectHit : Point -> Rectangle -> Bool
isBoxRectHit pos rect =
  pos.x > 0 &&
  pos.x < rect.x2 - rect.x1 &&
  pos.y > 0 &&
  pos.y < rect.y2 - rect.y1



-- TOOLBAR


toolbarPos : BoxId -> Model -> ToolbarPos
toolbarPos mapId model =
  let
    rect = (TM.byId mapId model).rect
  in
  ToolbarPos
    (\topic ->
      let
        topicPos = TM.topicPos topic.id mapId model
      in
      Point
        (topicPos.x - rect.x1 - C.topicW2)
        (topicPos.y - rect.y1 - C.topicH2 - 29) -- TODO: 29 ≈ toolbar height
    )
    (\assoc ->
      let
        (p1, p2) = TM.assocGeometry assoc mapId model
      in
      Point
        ((p1.x + p2.x) // 2 - rect.x1 - 32) -- TODO: 32 ≈ toolbar width / 2
        ((p1.y + p2.y) // 2 - rect.y1 - 13) -- TODO: 13 ≈ toolbar height / 2
    )
