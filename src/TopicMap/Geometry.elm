module TopicMap.Geometry exposing (findTopicAt)

import Box
import BoxRenderer exposing (TopicGeometry)
import Config as C
import Item
import Model exposing (Model)
import ModelBase exposing (..)
import TopicMap.TopicMap as TM
import TopicMap.TopicMapDef exposing (TopicMap, MapItem)
import Utils as U



{-| Finds the topic/box at a given screen position.
Returns the found topic/box (Id) and its context (BoxPath), or Nothing.
If `excludeTopicId` is given that topic/box will be excluded from search.
-}
findTopicAt : TopicGeometry -> Point -> Maybe Id -> Model -> Maybe (Id, BoxPath)
findTopicAt geometry pos excludeTopicId model =
  -- TODO: use geometry for nested boxes
  let
    initPos =
      Point
        (pos.x - C.whiteBoxPadding)
        (pos.y - C.whiteBoxPadding - C.appHeaderHeight)
  in
  testItemGeometry initPos model.boxId [] excludeTopicId model


testItemGeometry : Point -> Id -> BoxPath -> Maybe Id -> Model -> Maybe (Id, BoxPath)
testItemGeometry pos itemId boxPath excludeTopicId model =
  let
    maybeThisItem : Bool -> Maybe (Id, BoxPath)
    maybeThisItem found = if found then Just (itemId, boxPath) else Nothing
  in
  case TM.byIdOrNothing itemId model of
    Just map ->
      let
        items = TM.visibleTopics map
        relPos = mapRelPos pos map boxPath model
        testItems = testAllItems relPos items (itemId :: boxPath) excludeTopicId
      in
      -- test this map's items either if the map is displayed fullscreen, or it is whiteboxed
      if Box.isFullscreen itemId model then
        testItems model
      else
        let
          parentBoxId = Box.firstId boxPath -- Note: for a fullscreen box boxPath is empty
          isHeaderHovered = isTopicHeaderHovered pos map.id parentBoxId
          isRectHovered = isBoxRectHovered pos map parentBoxId
        in
        case Box.displayMode itemId parentBoxId model of
          Just (BoxD BlackBox) ->
            isHeaderHovered model |> maybeThisItem
          Just (BoxD WhiteBox) ->
            case testItems model of
              Just target -> Just target -- found nested (not this) item
              Nothing ->
                isHeaderHovered model ||
                isRectHovered model |> maybeThisItem
          _ -> U.logError "testItemGeometry" "Unexpected box display mode" Nothing
    Nothing ->
      -- hover test depends on topic's display mode
      let
        parentBoxId = Box.firstId boxPath -- Note: a topic is never displayed fullscreen
        isHeaderHovered = isTopicHeaderHovered pos itemId parentBoxId
        isDetailHovered = isTopicDetailHovered pos itemId parentBoxId
      in
      case Box.displayMode itemId parentBoxId model of
        Just (TopicD LabelOnly) ->
          isHeaderHovered model |> maybeThisItem
        Just (TopicD Detail) ->
          isHeaderHovered model ||
          isDetailHovered model |> maybeThisItem
        _ -> U.logError "testItemGeometry" "Unexpected topic display mode" Nothing


testAllItems : Point -> List MapItem -> BoxPath -> Maybe Id -> Model -> Maybe (Id, BoxPath)
testAllItems pos items boxPath excludeTopicId model =
  case items of
    [] -> Nothing
    item :: tailItems ->
      let
        testItem = testItemGeometry pos item.id boxPath excludeTopicId -- recursion
        testTailItems = testAllItems pos tailItems boxPath excludeTopicId -- recursion
      in
      -- return item if successfully tested AND not excluded by filter
      case (testItem model, excludeTopicId) of
        (Just ((targetId, _) as target), Just topicId) ->
          case targetId /= topicId of
            True -> Just target
            False -> testTailItems model
        (Just target, Nothing) -> Just target
        (Nothing, _) -> testTailItems model


{-| Transforms the given client position to a map-relative position according to the given map.
-}
mapRelPos : Point -> TopicMap -> BoxPath -> Model -> Point
mapRelPos pos map boxPath model =
  case Box.isFullscreen map.id model of -- For a fullscreen box boxPath is empty
    True -> pos
    False ->
      case TM.topicPos map.id (Box.firstId boxPath) model of
        Just boxPos ->
          Point
            (pos.x - boxPos.x + map.rect.x1 + C.topicW2)
            (pos.y - boxPos.y + map.rect.y1 - C.topicH2)
        Nothing -> pos


-- TODO: factor out common TopicMap.Size code

isTopicHeaderHovered : Point -> Id -> BoxId -> Model -> Bool
isTopicHeaderHovered pos topicId boxId model =
  case TM.topicPos topicId boxId model of
    Just topicPos ->
      pos.x > topicPos.x - C.topicW2 - C.topicHeight && -- left edge includes caret area
      pos.x < topicPos.x + C.topicW2 &&
      pos.y > topicPos.y - C.topicH2 &&
      pos.y < topicPos.y + C.topicH2
    Nothing -> False


isTopicDetailHovered : Point -> Id -> BoxId -> Model -> Bool
isTopicDetailHovered pos topicId boxId model =
  case (TM.topicPos topicId boxId model, Item.topicSize topicId .view model) of
    (Just topicPos, Just size) ->
      pos.x > topicPos.x - C.topicW2 + C.topicHeight &&
      pos.x < topicPos.x - C.topicW2 + C.topicHeight + size.w &&
      pos.y > topicPos.y - C.topicH2 &&
      pos.y < topicPos.y - C.topicH2 + size.h
    _ -> False


isBoxRectHovered : Point -> TopicMap -> BoxId -> Model -> Bool
isBoxRectHovered pos map parentBoxId model =
  case TM.topicPos map.id parentBoxId model of
    Just boxPos ->
      pos.x > boxPos.x - C.topicW2 &&
      pos.x < boxPos.x - C.topicW2 + map.rect.x2 - map.rect.x1 &&
      pos.y > boxPos.y + C.topicH2 &&
      pos.y < boxPos.y + C.topicH2 + map.rect.y2 - map.rect.y1
    Nothing -> False
