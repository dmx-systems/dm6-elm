module TopicMap.Size exposing (autoSize)

import Box
import Config as C
import Env exposing (ExtManager, Env2)
import Feature.TextDef exposing (EditState(..))
import Model exposing (Model)
import ModelBase exposing (..)
import Topic
import TopicMap.MouseDef exposing (DragState(..), DragMode(..))
import TopicMap.TopicMap as TM
import TopicMap.TopicMapDef exposing (MapTopic)
import TopicMap.ViewModel as VM
import Utils as U



-- UPDATE


{-| Calculates the TopicMap's "rect" (recursively) and modifies the model accordingly.
Returns the modified model along with, for convenience, the calculated rect.
Based on the rect's change the TopicMap's topic position adjustment within the parent
TopicMap (if any) is calculated as well.
-}
autoSize : BoxPath -> Env2 -> (Rectangle, Model)
autoSize boxPath ({model, ext}) =
  let
    boxId = Box.firstId boxPath
  in
  case TM.byId boxId model of
    Just map ->
      let
        topics = VM.topicsToRender map model
        (rect, model_) =
          if topics |> List.isEmpty then
            (C.whiteBoxEmpty, model)
          else
            topics |> List.foldr
              (\mapItem (rectAcc, modelAcc) ->
                accumulateItem mapItem boxPath rectAcc ext modelAcc
              )
              (Rectangle 0 0 0 0, model)
        newRect = addBoxPadding rect
      in
      ( newRect
      , updateBoxGeometry boxPath newRect map.rect model_
      )
    Nothing -> (Rectangle 0 0 0 0, model)


accumulateItem : MapTopic -> BoxPath -> Rectangle -> ExtManager -> Model -> (Rectangle, Model)
accumulateItem mapItem boxPath rectAcc ext model =
  let
    (rect, model_) = calcItemRect mapItem boxPath ext model
  in
  (accumulateRect rectAcc rect, model_)


calcItemRect : MapTopic -> BoxPath -> ExtManager -> Model -> (Rectangle, Model)
calcItemRect ({pos, expansion} as topic) boxPath ext model =
  case (Topic.isBox topic.id model, expansion) of
    (False, Collapsed) -> (topicExtent pos, model)
    (False, Expanded) -> (detailTopicExtent topic.id boxPath pos model, model)
    (True, Collapsed) -> (topicExtent pos, model)
    (True, Expanded) ->
      let
        (rect_, model_) = ext.autoSize (BoxId topic.id :: boxPath) model -- recursion
      in
      (boxExtent pos rect_, model_)


topicExtent : Point -> Rectangle
topicExtent pos =
  Rectangle
    (pos.x - C.topicW2)
    (pos.y - C.topicH2)
    (pos.x + C.topicW2 + 2 * C.topicBorderWidth)
    (pos.y + C.topicH2 + 2 * C.topicBorderWidth)


detailTopicExtent : TopicId -> BoxPath -> Point -> Model -> Rectangle
detailTopicExtent topicId boxPath pos model =
  let
    isEdit = model.text.edit == Edit topicId boxPath -- TODO: use Text (cyclic atm)
    get = if isEdit then .editor else .view
    maybeSize =
      case Topic.size topicId get model of
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
        (pos.x - C.topicW2 + size.w + C.topicHeight + 2 * C.topicBorderWidth)
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
            -- already and position adjustment would calculate 0
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
    |> TM.updateRect boxId (\_ -> rect)


adjustBoxPos : BoxId -> BoxId -> Rectangle -> Rectangle -> Model -> Model
adjustBoxPos (BoxId topicId) parentBoxId newRect oldRect model =
  model
    |> TM.updateTopicPos topicId parentBoxId
      (\oldPos ->
        (Point
          (oldPos.x + newRect.x1 - oldRect.x1)
          (oldPos.y + newRect.y1 - oldRect.y1)
        )
      )
