module Box.Geometry exposing (pointerTarget)

import Box
import Config as C
import Model exposing (Model)
import ModelParts exposing (..)



pointerTarget : Point -> Maybe Id -> Model -> Maybe (Id, BoxPath)
pointerTarget pos filterTopicId model =
  let
    initPos =
      Point
        (pos.x - C.whiteBoxPadding)
        (pos.y - C.whiteBoxPadding - C.appHeaderHeight)
  in
  findTarget model.boxId [] initPos filterTopicId model


-- For the fullscreen box boxPath is empty
findTarget : Id -> BoxPath -> Point -> Maybe Id -> Model -> Maybe (Id, BoxPath)
findTarget itemId boxPath pos filterTopicId model =
  case Box.byId itemId model of
    Just box ->
      let
        items = box |> Box.visibleTopics
        relPos = boxRelPos pos box boxPath model
      in
      case findInBox items (itemId :: boxPath) relPos filterTopicId model of
        Just target -> Just target
        Nothing ->
          case itemId == model.boxId of
            True -> Nothing
            False ->
              case isBoxHover box (Box.firstId boxPath) pos model of
                True -> Just (itemId, boxPath)
                False -> Nothing
    Nothing ->
      case isTopicHover itemId (Box.firstId boxPath) pos model of
        True -> Just (itemId, boxPath)
        False -> Nothing


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


isBoxHover : Box -> BoxId -> Point -> Model -> Bool
isBoxHover box boxId pos model =
  isTopicHover box.id boxId pos model -- TODO: whitebox display


isTopicHover : Id -> BoxId -> Point -> Model -> Bool
isTopicHover topicId boxId pos model =
  -- TODO: detail display
  case (Box.byIdOrLog boxId model, Box.topicPos topicId boxId model) of
    (Just box, Just tp) ->
      pos.x > tp.x - C.topicW2 - C.topicHeight && -- left edge includes caret area
      pos.x < tp.x + C.topicW2 &&
      pos.y > tp.y - C.topicH2 &&
      pos.y < tp.y + C.topicH2
    _ -> False
