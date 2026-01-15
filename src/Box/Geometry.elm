module Box.Geometry exposing (pointerTarget)

import Box
import Config as C
import Model exposing (Model)
import ModelParts exposing (..)



pointerTarget : Point -> Model -> Maybe (Id, BoxPath)
pointerTarget pos model =
  findTarget model.boxId [] pos model


-- For the fullscreen box boxPath is empty
findTarget : Id -> BoxPath -> Point -> Model -> Maybe (Id, BoxPath)
findTarget itemId boxPath pos model =
  case Box.byId itemId model of
    Just box ->
      case findInItems (Box.visibleTopics box) (itemId :: boxPath) pos model of
        Just target -> Just target
        Nothing ->
          case isBoxHover box pos of
            True -> Just (itemId, boxPath)
            False -> Nothing
    Nothing ->
      case isTopicHover itemId (Box.firstId boxPath) pos model of
        True -> Just (itemId, boxPath)
        False -> Nothing


findInItems : List BoxItem -> BoxPath -> Point -> Model -> Maybe (Id, BoxPath)
findInItems items boxPath pos model =
  case items of
    [] -> Nothing
    item :: tailItems ->
      case findTarget item.id boxPath pos model of -- recursion
        Just target -> Just target
        Nothing -> findInItems tailItems boxPath pos model -- recursion


isBoxHover : Box -> Point -> Bool
isBoxHover box pos =
  False -- TODO: check box rect


isTopicHover : Id -> BoxId -> Point -> Model -> Bool
isTopicHover topicId boxId pos model =
  -- TODO: display mode
  case (Box.byIdOrLog boxId model, Box.topicPos topicId boxId model) of
    (Just box, Just tp) ->
      pos.x > tp.x - C.topicW2 - box.rect.x1 &&
      pos.x < tp.x + C.topicW2 - box.rect.x1 &&
      pos.y - C.appHeaderHeight > tp.y - C.topicH2 - box.rect.y1 &&
      pos.y - C.appHeaderHeight < tp.y + C.topicH2 - box.rect.y1
    _ -> False
