module Box.Geometry exposing (pointerTarget)

import Box
import Config as C
import Model exposing (Model)
import ModelParts exposing (..)



pointerTarget : Point -> Maybe Id -> Model -> Maybe (Id, BoxPath)
pointerTarget pos filterTopicId model =
  findTarget model.boxId [] pos filterTopicId model


-- For the fullscreen box boxPath is empty
findTarget : Id -> BoxPath -> Point -> Maybe Id -> Model -> Maybe (Id, BoxPath)
findTarget itemId boxPath pos filterTopicId model =
  case Box.byId itemId model of
    Just box ->
      case findInItems (Box.visibleTopics box) (itemId :: boxPath) pos filterTopicId model of
        Just target -> Just target
        Nothing ->
          case itemId == homeBoxId of
            True -> Nothing
            False ->
              case isBoxHover box (Box.firstId boxPath) pos model of
                True -> Just (itemId, boxPath)
                False -> Nothing
    Nothing ->
      case isTopicHover itemId (Box.firstId boxPath) pos model of
        True -> Just (itemId, boxPath)
        False -> Nothing


findInItems : List BoxItem -> BoxPath -> Point -> Maybe Id -> Model -> Maybe (Id, BoxPath)
findInItems items boxPath pos filterTopicId model =
  case items of
    [] -> Nothing
    item :: tailItems ->
      let
        maybeTarget = findTarget item.id boxPath pos filterTopicId model -- recursion
        continueSearch = findInItems tailItems boxPath pos filterTopicId -- recursion
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
  isTopicHover box.id boxId pos model -- TODO


isTopicHover : Id -> BoxId -> Point -> Model -> Bool
isTopicHover topicId boxId pos model =
  -- TODO: display mode
  case (Box.byIdOrLog boxId model, Box.topicPos topicId boxId model) of
    (Just box, Just tp) ->
      pos.x > tp.x - C.topicW2 - box.rect.x1 - C.topicHeight && -- left edge includes caret area
      pos.x < tp.x + C.topicW2 - box.rect.x1 &&
      pos.y - C.appHeaderHeight > tp.y - C.topicH2 - box.rect.y1 &&
      pos.y - C.appHeaderHeight < tp.y + C.topicH2 - box.rect.y1
    _ -> False
