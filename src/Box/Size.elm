module Box.Size exposing (auto)

import Box
import Config as C
import Feature.Mouse exposing (DragState(..), DragMode(..))
import Feature.Text exposing (EditState(..))
import Item
import Map.Model as MM
import Model exposing (Model)
import ModelParts exposing (..)
import Utils as U



-- UPDATE


auto : Model -> Model
auto model =
  model
  |> calcBoxRect [ model.boxId ]
  |> Tuple.second


{-| Calculates the box's "rect" (recursively) and modifies the model accordingly.
Returns the modified model along with, for convenience, the calculated rect.
Based on the rect's change the box topic position adjustment within the parent box (if any) is
calculated as well.
-}
calcBoxRect : BoxPath -> Model -> (Rectangle, Model)
calcBoxRect boxPath model =
  let
    boxId = Box.firstId boxPath
  in
  case Box.byIdOrLog boxId model of
    Just box ->
      let
        (rect, model_) =
          MM.topicsToRender box model |> List.foldr
            (\boxItem (rectAcc, modelAcc) ->
              accumulateItem boxItem boxPath rectAcc modelAcc
            )
            (Rectangle 0 0 0 0, model)
        newRect = addBoxPadding rect
      in
      ( newRect
      , updateBoxGeometry boxPath newRect box.rect model_
      )
    Nothing -> (Rectangle 0 0 0 0, model)


accumulateItem : BoxItem -> BoxPath -> Rectangle -> Model -> (Rectangle, Model)
accumulateItem boxItem boxPath rectAcc model =
  let
    (rect, model_) = calcItemRect boxItem boxPath model
  in
  (accumulateRect rectAcc rect, model_)


calcItemRect : BoxItem -> BoxPath -> Model -> (Rectangle, Model)
calcItemRect boxItem boxPath model =
  case boxItem.props of
    TopicP {pos, displayMode} ->
      case displayMode of
        TopicD LabelOnly -> (topicExtent pos, model)
        TopicD Detail -> (detailTopicExtent boxItem.id boxPath pos model, model)
        BoxD BlackBox -> (topicExtent pos, model)
        BoxD WhiteBox ->
          let
            (rect_, model_) = calcBoxRect (boxItem.id :: boxPath) model -- recursion
          in
          (boxExtent pos rect_, model_)
        BoxD Unboxed -> (topicExtent pos, model)
    AssocP _ -> (Rectangle 0 0 0 0, model) -- never called


topicExtent : Point -> Rectangle
topicExtent pos =
  Rectangle
    (pos.x - C.topicW2)
    (pos.y - C.topicH2)
    (pos.x + C.topicW2 + 2 * C.topicBorderWidth)
    (pos.y + C.topicH2 + 2 * C.topicBorderWidth)


detailTopicExtent : Id -> BoxPath -> Point -> Model -> Rectangle
detailTopicExtent topicId boxPath pos model =
  let
    isEdit = model.text.edit == Edit topicId boxPath -- TODO: use TextAPI (cyclic atm)
    get = if isEdit then .editor else .view
    maybeSize =
      case Item.topicSize topicId get model of
        Just size ->
          if isEdit then
            Just { size | w = C.topicDetailMaxWidth }
          else
            Just size
        Nothing -> Nothing
  in
  case maybeSize of
    Just size ->
      Rectangle
        (pos.x - C.topicW2)
        (pos.y - C.topicH2)
        (pos.x - C.topicW2 + size.w + C.topicSize.h + 2 * C.topicBorderWidth)
        (pos.y - C.topicH2 + size.h + 2 * C.topicBorderWidth)
    Nothing -> Rectangle 0 0 0 0 -- error is logged already


boxExtent : Point -> Rectangle -> Rectangle
boxExtent pos rect =
  let
    boxWidth = rect.x2 - rect.x1
    boxHeight = rect.y2 - rect.y1
  in
  Rectangle
    (pos.x - C.topicW2)
    (pos.y - C.topicH2)
    (pos.x - C.topicW2 + boxWidth)
    (pos.y + C.topicH2 + boxHeight)


accumulateRect : Rectangle -> Rectangle -> Rectangle
accumulateRect rectAcc rect =
  Rectangle
    (min rectAcc.x1 rect.x1)
    (min rectAcc.y1 rect.y1)
    (max rectAcc.x2 rect.x2)
    (max rectAcc.y2 rect.y2)


addBoxPadding : Rectangle -> Rectangle
addBoxPadding rect =
  Rectangle
    (rect.x1 - C.whiteBoxPadding)
    (rect.y1 - C.whiteBoxPadding)
    (rect.x2 + C.whiteBoxPadding)
    (rect.y2 + C.whiteBoxPadding)


{-| Sets the box's "newRect" and, based on its change, calculates and sets the box's "pos"
adjustment ("delta")
-}
updateBoxGeometry : BoxPath -> Rectangle -> Rectangle -> Model -> Model
updateBoxGeometry boxPath newRect oldRect model =
  case boxPath of
    boxId :: parentBoxId :: _ ->
      let
        (isDragInProgress, isOnDragPath, isBoxInDragPath) =
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
    [] -> U.logError "updateBoxGeometry" "boxPath is empty!" model


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
