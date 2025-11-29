module Box.Size exposing (auto)

import Box
import Config as C
import Feature.Mouse exposing (DragState(..), DragMode(..))
import Feature.TextEdit exposing (EditState(..))
import Model exposing (Model)
import ModelParts exposing (..)
import Utils as U

import Dict



-- UPDATE


auto : Model -> Model
auto model =
  calcBoxRect [ Box.active model ] model |> Tuple.second


{-| Calculates (recursively) the box's "rect"
-}
calcBoxRect : BoxPath -> Model -> (Rectangle, Model)
calcBoxRect boxPath model =
  let
    boxId = Box.firstId boxPath
  in
  case Box.byIdOrLog boxId model.boxes of
    Just box ->
      let
        (rect, model_) =
          (box.items |> Dict.values |> List.filter Box.isVisible |> List.foldr
            (\boxItem (rectAcc, modelAcc) ->
              calcItemSize boxItem boxPath rectAcc modelAcc
            )
            (Rectangle 5000 5000 -5000 -5000, model) -- x-min y-min x-max y-max
            -- FIXME: if box is empty its rect size is negative
          )
        newRect = Rectangle
          (rect.x1 - C.whiteBoxPadding)
          (rect.y1 - C.whiteBoxPadding)
          (rect.x2 + C.whiteBoxPadding)
          (rect.y2 + C.whiteBoxPadding)
      in
      ( newRect
      , storeBoxGeometry boxPath newRect box.rect model_
      )
    Nothing -> (Rectangle 0 0 0 0, model)


calcItemSize : BoxItem -> BoxPath -> Rectangle -> Model -> (Rectangle, Model)
calcItemSize boxItem pathToParent rectAcc model =
  let
    boxId = Box.firstId pathToParent
  in
  case boxItem.props of
    TopicV {pos, size, displayMode} ->
      case displayMode of
        TopicD LabelOnly -> (topicExtent pos rectAcc, model)
        TopicD Detail -> (detailTopicExtent boxItem.id boxId pos size rectAcc model, model)
        BoxD BlackBox -> (topicExtent pos rectAcc, model)
        BoxD WhiteBox ->
          let
            (rect, model_) = calcBoxRect (boxItem.id :: pathToParent) model -- recursion
          in
          (boxExtent pos rect rectAcc, model_)
        BoxD Unboxed -> (topicExtent pos rectAcc, model)
    AssocV _ -> (rectAcc, model)


{-| Sets the box's "newRect" and, based on its change, calculates and sets the box's "pos"
adjustmennt ("delta")
-}
storeBoxGeometry : BoxPath -> Rectangle -> Rectangle -> Model -> Model
storeBoxGeometry boxPath newRect oldRect model =
  case boxPath of
    boxId :: parentBoxId :: _ ->
      let
        (isDragInProgress, isOnDragPath, isBoxInDragPath) =
          case model.mouse.dragState of
            Drag DragTopic _ _ dragPath _ _ _ ->
              (True
              , (dragPath |> List.drop (List.length dragPath - List.length boxPath)) == boxPath
              , List.member boxId dragPath
              )
            _ -> (False, False, False)
      in
      if isDragInProgress then
        if isOnDragPath then
          model
          |> setBoxRect boxId newRect
          |> adjustBoxPos boxId parentBoxId newRect oldRect
          -- if boxes are revealed more than once only those within the drag-path
          -- get the position adjustment, the other box's positions remain stable
        else
          if isBoxInDragPath then
            model
            -- do nothing, postpone box's geometry update until reaching drag-path,
            -- otherwise, when reaching drag-path, the box's rect would be updated
            -- already and position adjustment will calculate 0
          else
            model |> setBoxRect boxId newRect
      else
        model |> setBoxRect boxId newRect
    [ boxId ] ->
      model |> setBoxRect boxId newRect
    [] -> U.logError "storeBoxGeometry" "boxPath is empty!" model


setBoxRect : BoxId -> Rectangle -> Model -> Model
setBoxRect boxId rect model =
  model
  |> Box.updateRect boxId (\_ -> rect)


adjustBoxPos : BoxId -> BoxId -> Rectangle -> Rectangle -> Model -> Model
adjustBoxPos boxId parentBoxId newRect oldRect model =
  model |> Box.setTopicPosByDelta boxId parentBoxId
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
      if model.edit.state == ItemEdit topicId boxId then
        C.topicDetailMaxWidth
      else
        size.w
  in
  Rectangle
    (min rectAcc.x1 (pos.x - C.topicW2))
    (min rectAcc.y1 (pos.y - C.topicH2))
    (max rectAcc.x2 (pos.x - C.topicW2 + textWidth + C.topicSize.h + 2 * C.topicBorderWidth))
    (max rectAcc.y2 (pos.y - C.topicH2 + size.h + 2 * C.topicBorderWidth))


boxExtent : Point -> Rectangle -> Rectangle -> Rectangle
boxExtent pos rect rectAcc =
  let
    boxWidth = rect.x2 - rect.x1
    boxHeight = rect.y2 - rect.y1
  in
  Rectangle
    (min rectAcc.x1 (pos.x - C.topicW2))
    (min rectAcc.y1 (pos.y - C.topicH2))
    (max rectAcc.x2 (pos.x - C.topicW2 + boxWidth))
    (max rectAcc.y2 (pos.y + C.topicH2 + boxHeight))
