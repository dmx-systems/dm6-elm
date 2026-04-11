module TopicMap.Geometry exposing (hitTest)

import Box
import Config as C
import ExtensionDef exposing (HitTest)
import Item
import Model exposing (Model)
import ModelBase exposing (..)
import TopicMap.TopicMap as TM
import TopicMap.TopicMapDef exposing (TopicMap, MapItem)
import Utils as U



type alias MapItems =
  List MapItem


{-| Finds the topic/box at a given screen position.
Returns the found topic/box (Id) and its context (BoxPath), or Nothing.
If `excludeTopicId` is given that topic/box will be excluded from search.
-}
hitTest : BoxId -> BoxPath -> Point -> Maybe Id -> HitTest -> Model -> Maybe Target
hitTest boxId boxPath pos excludeTopicId hitTest_ model =
  case TM.byId boxId model of
    Just map ->
      let
        items = TM.visibleTopics map
        relPos = mapOffset pos map
      in
      case testChildren relPos items (boxId :: boxPath) excludeTopicId hitTest_ model of
        Just target -> Just target
        Nothing ->
          if Box.isFullscreen boxId model then
            Nothing
          else
            let
              parentBoxId = Box.firstId boxPath
            in
            if isBoxRectHit pos map parentBoxId model then
              Just (boxId, boxPath)
            else
              Nothing
    Nothing -> Nothing


testChildren : Point -> MapItems -> BoxPath -> Maybe Id -> HitTest -> Model -> Maybe Target
testChildren pos items boxPath excludeTopicId hitTest_ model =
  case items of
    [] -> Nothing
    item :: tailItems ->
      let
        maybeItem : Bool -> Maybe Target
        maybeItem found = if found then Just (item.id, boxPath) else Nothing
        --
        boxId = Box.firstId boxPath
        isHeaderHit = isTopicHeaderHit pos item.id boxId
        relPos = relPos_ pos item.id boxPath
        maybeTarget =
          if Item.isBox item.id model then
            case Box.displayMode item.id boxId model of
              Just (BoxD BlackBox) ->
                isHeaderHit model |> maybeItem
              Just (BoxD WhiteBox) ->
                case hitTest_ item.id boxPath (relPos model) excludeTopicId model of
                  Just target -> Just target
                  Nothing ->
                    isHeaderHit model |> maybeItem
              _ -> U.logError "TopicMap.Geometry.testChildren" "Unexpected box display mode"
                Nothing
          else
            isTopicHit item.id boxPath pos model |> maybeItem
        -- recursion
        testTailItems = testChildren pos tailItems boxPath excludeTopicId hitTest_
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
    parentBoxId = Box.firstId boxPath -- Note: a topic is never displayed fullscreen
    isHeaderHit = isTopicHeaderHit pos itemId parentBoxId
    isDetailHit = isTopicDetailHit pos itemId parentBoxId
  in
  -- test depends on topic's display mode
  case Box.displayMode itemId parentBoxId model of
    Just (TopicD LabelOnly) ->
      isHeaderHit model
    Just (TopicD Detail) ->
      isHeaderHit model || isDetailHit model
    _ -> U.logError "TopicMap.Geometry.isTopicHit" "Unexpected topic display mode" False


{-| Transforms the given screen position to a map-relative position according to the given map.
-}
mapOffset : Point -> TopicMap -> Point
mapOffset pos map =
  Point
    (pos.x + map.rect.x1 + map.scroll.x)
    (pos.y + map.rect.y1 + map.scroll.y)


relPos_ : Point -> BoxId -> BoxPath -> Model -> Point
relPos_ pos boxId boxPath model =
  case TM.topicPos boxId (Box.firstId boxPath) model of
    Just boxPos ->
      Point
        (pos.x - boxPos.x + C.topicW2)
        (pos.y - boxPos.y - C.topicH2)
    Nothing -> pos


-- TODO: factor out common TopicMap.Size code

isTopicHeaderHit : Point -> Id -> BoxId -> Model -> Bool
isTopicHeaderHit pos topicId boxId model =
  case TM.topicPos topicId boxId model of
    Just topicPos ->
      pos.x > topicPos.x - C.topicW2 - C.topicHeight && -- left edge includes caret area
      pos.x < topicPos.x + C.topicW2 &&
      pos.y > topicPos.y - C.topicH2 &&
      pos.y < topicPos.y + C.topicH2
    Nothing -> False


isTopicDetailHit : Point -> Id -> BoxId -> Model -> Bool
isTopicDetailHit pos topicId boxId model =
  case (TM.topicPos topicId boxId model, Item.topicSize topicId .view model) of
    (Just topicPos, Just size) ->
      pos.x > topicPos.x - C.topicW2 + C.topicHeight && -- topicHeight = icon box width
      pos.x < topicPos.x - C.topicW2 + C.topicHeight + size.w &&
      pos.y > topicPos.y - C.topicH2 &&
      pos.y < topicPos.y - C.topicH2 + size.h
    _ -> False


isBoxRectHit : Point -> TopicMap -> BoxId -> Model -> Bool
isBoxRectHit pos map parentBoxId model =
  case TM.topicPos map.id parentBoxId model of
    Just boxPos ->
      pos.x > 0 &&
      pos.x < map.rect.x2 - map.rect.x1 &&
      pos.y > 0 &&
      pos.y < map.rect.y2 - map.rect.y1
    Nothing -> False
