module MapAutoSize exposing (autoSize)

import AppModel exposing (..)
import Config as C
import Model exposing (..)
import ModelAPI exposing (..)
import Mouse exposing (DragState(..), DragMode(..))
import Utils exposing (logError)

import Dict



-- UPDATE


autoSize : Model -> Model
autoSize model =
  calcMapRect [ activeMap model ] model |> Tuple.second


{-| Calculates (recursively) the box's "rect"
-}
calcMapRect : BoxPath -> Model -> (Rectangle, Model)
calcMapRect boxPath model =
  let
    boxId = firstId boxPath
  in
  case mapByIdOrLog boxId model.boxes of
    Just box ->
      let
        (rect, model_) =
          (box.items |> Dict.values |> List.filter isVisible |> List.foldr
            (\mapItem (rectAcc, modelAcc) ->
              calcItemSize mapItem boxPath rectAcc modelAcc
            )
            (Rectangle 5000 5000 -5000 -5000, model) -- x-min y-min x-max y-max
          )
        newRect = Rectangle
          (rect.x1 - C.whiteBoxPadding)
          (rect.y1 - C.whiteBoxPadding)
          (rect.x2 + C.whiteBoxPadding)
          (rect.y2 + C.whiteBoxPadding)
      in
      ( newRect
      , storeMapGeometry boxPath newRect box.rect model_
      )
    Nothing -> (Rectangle 0 0 0 0, model)


calcItemSize : BoxItem -> BoxPath -> Rectangle -> Model -> (Rectangle, Model)
calcItemSize mapItem pathToParent rectAcc model =
  let
    boxId = firstId pathToParent
  in
  case mapItem.props of
    TopicV {pos, size, displayMode} ->
      case displayMode of
        TopicD LabelOnly -> (topicExtent pos rectAcc, model)
        TopicD Detail -> (detailTopicExtent mapItem.id boxId pos size rectAcc model, model)
        BoxD BlackBox -> (topicExtent pos rectAcc, model)
        BoxD WhiteBox ->
          let
            (rect, model_) = calcMapRect (mapItem.id :: pathToParent) model -- recursion
          in
          (mapExtent pos rect rectAcc, model_)
        BoxD Unboxed -> (topicExtent pos rectAcc, model)
    AssocV _ -> (rectAcc, model)


{-| Stores the box's "newRect" and, based on its change, calculates and stores the box's "pos"
adjustmennt ("delta")
-}
storeMapGeometry : BoxPath -> Rectangle -> Rectangle -> Model -> Model
storeMapGeometry boxPath newRect oldRect model =
  case boxPath of
    boxId :: parentMapId :: _ ->
      let
        (isDragInProgress, isOnDragPath, isMapInDragPath) =
          case model.mouse.dragState of
            Drag DragTopic _ dragPath _ _ _ ->
              (True
              , (dragPath |> List.drop (List.length dragPath - List.length boxPath)) == boxPath
              , List.member boxId dragPath
              )
            _ -> (False, False, False)
      in
      if isDragInProgress then
        if isOnDragPath then
          model
          |> storeMapRect boxId newRect
          |> adjustMapPos boxId parentMapId newRect oldRect
          -- if boxes are revealed more than once only those within the drag-path
          -- get the position adjustment, the other box's positions remain stable
        else
          if isMapInDragPath then
            model
            -- do nothing, postpone box's geometry update until reaching drag-path,
            -- otherwise, when reaching drag-path, the box's rect would be updated
            -- already and position adjustment will calculate 0
          else
            model |> storeMapRect boxId newRect
      else
        model |> storeMapRect boxId newRect
    [_] -> model -- do nothing, for the fullscreen box there is no geometry update
    [] -> logError "storeMapGeometry" "boxPath is empty!" model


storeMapRect : BoxId -> Rectangle -> Model -> Model
storeMapRect boxId newRect model =
  model |> updateMapRect boxId (\rect -> newRect)


adjustMapPos : BoxId -> BoxId -> Rectangle -> Rectangle -> Model -> Model
adjustMapPos boxId parentMapId newRect oldRect model =
  model |> setTopicPosByDelta boxId parentMapId
    (Point
      (newRect.x1 - oldRect.x1)
      (newRect.y1 - oldRect.y1)
    )


topicExtent : Point -> Rectangle -> Rectangle
topicExtent pos rectAcc =
  Rectangle
    (min rectAcc.x1 (pos.x - C.topicW2))
    (min rectAcc.y1 (pos.y - C.topicH2))
    (max rectAcc.x2 (pos.x + C.topicW2 + 2 * C.topicBorderWidth))
    (max rectAcc.y2 (pos.y + C.topicH2 + 2 * C.topicBorderWidth))


detailTopicExtent : Id -> BoxId -> Point -> Size -> Rectangle -> Model -> Rectangle
detailTopicExtent topicId boxId pos size rectAcc model =
  let
    textWidth =
      if model.editState == ItemEdit topicId boxId then
        C.topicDetailMaxWidth
      else
        size.w
  in
  Rectangle
    (min rectAcc.x1 (pos.x - C.topicW2))
    (min rectAcc.y1 (pos.y - C.topicH2))
    (max rectAcc.x2 (pos.x - C.topicW2 + textWidth + C.topicSize.h + 2 * C.topicBorderWidth))
    (max rectAcc.y2 (pos.y - C.topicH2 + size.h + 2 * C.topicBorderWidth))


mapExtent : Point -> Rectangle -> Rectangle -> Rectangle
mapExtent pos rect rectAcc =
  let
    mapWidth = rect.x2 - rect.x1
    mapHeight = rect.y2 - rect.y1
  in
  Rectangle
    (min rectAcc.x1 (pos.x - C.topicW2))
    (min rectAcc.y1 (pos.y - C.topicH2))
    (max rectAcc.x2 (pos.x - C.topicW2 + mapWidth))
    (max rectAcc.y2 (pos.y + C.topicH2 + mapHeight))
