module TopicMap.Geometry exposing (pointerTarget)

import Box
import Config as C
import Item
import Model exposing (Model)
import ModelParts exposing (Id, BoxId, BoxPath, Point)
import TopicMap.TopicMap as TM
import TopicMap.TopicMapDef exposing (TopicMap, MapItem, DisplayMode(..), TopicDisplay(..),
  BoxDisplay(..))
import Utils as U



{- Searches the target for a pointer position.
Returns the target (topic/box Id) and its context (BoxPath), or Nothing -}
pointerTarget : Point -> Maybe Id -> Model -> Maybe (Id, BoxPath)
pointerTarget pos filterTopicId model =
  let
    initPos =
      Point
        (pos.x - C.whiteBoxPadding)
        (pos.y - C.whiteBoxPadding - C.appHeaderHeight)
  in
  searchInItem initPos model.boxId [] filterTopicId model


{- Searches the target for a pointer position, starting at the given item (topic/box Id)
and context (BoxPath). -}
searchInItem : Point -> Id -> BoxPath -> Maybe Id -> Model -> Maybe (Id, BoxPath)
searchInItem pos itemId boxPath filterTopicId model =
  let
    maybeThisItem : Bool -> Maybe (Id, BoxPath)
    maybeThisItem found = if found then Just (itemId, boxPath) else Nothing
  in
  case TM.byId itemId model of
    Just box ->
      let
        items = TM.visibleTopics box
        relPos = boxRelPos pos box boxPath model
        searchBox = searchInItems relPos items (itemId :: boxPath) filterTopicId
      in
      -- Note: for a fullscreen box boxPath is empty
      case TM.isFullscreen itemId model of
        True -> searchBox model
        False ->
          let
            parentBoxId = Box.firstId boxPath
            isHeaderHovered = isTopicHeaderHovered pos box.id parentBoxId
            isRectHovered = isBoxRectHovered pos box parentBoxId
          in
          case TM.displayMode itemId parentBoxId model of
            Just (BoxD BlackBox) ->
              isHeaderHovered model |> maybeThisItem
            Just (BoxD WhiteBox) ->
              case searchBox model of
                Just target -> Just target -- found nested (not this) item
                Nothing ->
                  isHeaderHovered model ||
                  isRectHovered model |> maybeThisItem
            Just (BoxD Unboxed) ->
              isHeaderHovered model |> maybeThisItem
            _ -> U.logError "searchInItem" "Unexpected box display mode" Nothing
    Nothing ->
      let
        parentBoxId = Box.firstId boxPath
        isHeaderHovered = isTopicHeaderHovered pos itemId parentBoxId
        isDetailHovered = isTopicDetailHovered pos itemId parentBoxId
      in
      case TM.displayMode itemId parentBoxId model of
        Just (TopicD LabelOnly) ->
          isHeaderHovered model |> maybeThisItem
        Just (TopicD Detail) ->
          isHeaderHovered model ||
          isDetailHovered model |> maybeThisItem
        _ -> U.logError "searchInItem" "Unexpected topic display mode" Nothing


searchInItems : Point -> List MapItem -> BoxPath -> Maybe Id -> Model -> Maybe (Id, BoxPath)
searchInItems pos items boxPath filterTopicId model =
  case items of
    [] -> Nothing
    item :: tailItems ->
      let
        searchItem = searchInItem pos item.id boxPath filterTopicId -- recursion
        searchTailItems = searchInItems pos tailItems boxPath filterTopicId -- recursion
      in
      case (searchItem model, filterTopicId) of
        (Just ((targetId, _) as target), Just topicId) ->
          case targetId /= topicId of
            True -> Just target
            False -> searchTailItems model
        (Just target, Nothing) -> Just target
        (Nothing, _) -> searchTailItems model


-- For a fullscreen box boxPath is empty
boxRelPos : Point -> TopicMap -> BoxPath -> Model -> Point
boxRelPos pos box boxPath model =
  case TM.isFullscreen box.id model of
    True -> pos
    False ->
      case TM.topicPos box.id (Box.firstId boxPath) model of
        Just boxPos ->
          Point
            (pos.x - boxPos.x + box.rect.x1 + C.topicW2)
            (pos.y - boxPos.y + box.rect.y1 - C.topicH2)
        Nothing -> pos


-- TODO: factor out common Box.Size code

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
isBoxRectHovered pos box parentBoxId model =
  case TM.topicPos box.id parentBoxId model of
    Just boxPos ->
      pos.x > boxPos.x - C.topicW2 &&
      pos.x < boxPos.x - C.topicW2 + box.rect.x2 - box.rect.x1 &&
      pos.y > boxPos.y + C.topicH2 &&
      pos.y < boxPos.y + C.topicH2 + box.rect.y2 - box.rect.y1
    Nothing -> False
