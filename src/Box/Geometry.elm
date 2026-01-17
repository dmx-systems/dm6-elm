module Box.Geometry exposing (pointerTarget)

import Box
import Config as C
import Model exposing (Model)
import ModelParts exposing (..)
import Utils as U



pointerTarget : Point -> Maybe Id -> Model -> Maybe (Id, BoxPath)
pointerTarget pos filterTopicId model =
  let
    initPos =
      Point
        (pos.x - C.whiteBoxPadding)
        (pos.y - C.whiteBoxPadding - C.appHeaderHeight)
  in
  findTarget model.boxId [] initPos filterTopicId model


-- For a fullscreen box boxPath is empty
findTarget : Id -> BoxPath -> Point -> Maybe Id -> Model -> Maybe (Id, BoxPath)
findTarget itemId boxPath pos filterTopicId model =
  case Box.byId itemId model of
    Just box ->
      let
        items = box |> Box.visibleTopics
        relPos = boxRelPos pos box boxPath model
      in
      -- TODO: only search inside whiteboxes
      case findInBox items (itemId :: boxPath) relPos filterTopicId model of
        Just target -> Just target
        Nothing ->
          case itemId == model.boxId of
            True -> Nothing
            False ->
              case isInsideBox pos box (Box.firstId boxPath) model of
                True -> Just (itemId, boxPath)
                False -> Nothing
    Nothing ->
      case isInsideTopic pos itemId (Box.firstId boxPath) model of
        True -> Just (itemId, boxPath)
        False -> Nothing


-- For a fullscreen box boxPath is empty
boxRelPos : Point -> Box -> BoxPath -> Model -> Point
boxRelPos pos box boxPath model =
  case box.id == model.boxId of
    True -> pos
    False ->
      case Box.topicPos box.id (Box.firstId boxPath) model of
        Just boxPos ->
          Point
            (pos.x - boxPos.x + box.rect.x1 + C.topicW2)
            (pos.y - boxPos.y + box.rect.y1 - C.topicH2)
        Nothing -> pos


findInBox : List BoxItem -> BoxPath -> Point -> Maybe Id -> Model -> Maybe (Id, BoxPath)
findInBox items boxPath pos filterTopicId model =
  case items of
    [] -> Nothing
    item :: tailItems ->
      let
        maybeTarget = findTarget item.id boxPath pos filterTopicId model -- recursion
        continueSearch = findInBox tailItems boxPath pos filterTopicId -- recursion
      in
      case (maybeTarget, filterTopicId) of
        (Just ((targetId, _) as target), Just topicId) ->
          case targetId /= topicId of
            True -> Just target
            False -> continueSearch model
        (Just target, Nothing) -> Just target
        (Nothing, _) -> continueSearch model


isInsideTopic : Point -> Id -> BoxId -> Model -> Bool
isInsideTopic pos topicId boxId model =
  isInsideTopicHeader pos topicId boxId model -- TODO: detail display


isInsideBox : Point -> Box -> BoxId -> Model -> Bool
isInsideBox pos box parentBoxId model =
  let
    header = isInsideTopicHeader pos box.id parentBoxId model
  in
  case Box.displayMode box.id parentBoxId model of
    Just (BoxD WhiteBox) -> header || isInsideBoxRect pos box parentBoxId model
    Just (BoxD BlackBox) -> header
    Just (BoxD Unboxed) -> header
    _ -> U.logError "isInsideBox" "Unexpected box display mode" False


isInsideTopicHeader : Point -> Id -> BoxId -> Model -> Bool
isInsideTopicHeader pos topicId boxId model =
  case Box.topicPos topicId boxId model of
    Just topicPos ->
      pos.x > topicPos.x - C.topicW2 - C.topicHeight && -- left edge includes caret area
      pos.x < topicPos.x + C.topicW2 &&
      pos.y > topicPos.y - C.topicH2 &&
      pos.y < topicPos.y + C.topicH2
    Nothing -> False


isInsideBoxRect : Point -> Box -> BoxId -> Model -> Bool
isInsideBoxRect pos box parentBoxId model =
  case Box.topicPos box.id parentBoxId model of
    Just boxPos ->
      pos.x > boxPos.x - C.topicW2 &&
      pos.x < boxPos.x - C.topicW2 + box.rect.x2 - box.rect.x1 &&
      pos.y > boxPos.y + C.topicH2 &&
      pos.y < boxPos.y + C.topicH2 + box.rect.y2 - box.rect.y1
    Nothing -> False
