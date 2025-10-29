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


{-| Calculates (recursively) the map's "rect"
-}
calcMapRect : MapPath -> Model -> (Rectangle, Model)
calcMapRect mapPath model =
  let
    mapId = firstId mapPath
  in
  case mapByIdOrLog mapId model.maps of
    Just map ->
      let
        (rect, model_) =
          (map.items |> Dict.values |> List.filter isVisible |> List.foldr
            (\mapItem (rectAcc, modelAcc) ->
              calcItemSize mapItem mapPath rectAcc modelAcc
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
      , storeMapGeometry mapPath newRect map.rect model_
      )
    Nothing -> (Rectangle 0 0 0 0, model)


calcItemSize : MapItem -> MapPath -> Rectangle -> Model -> (Rectangle, Model)
calcItemSize mapItem pathToParent rectAcc model =
  let
    mapId = firstId pathToParent
  in
  case mapItem.props of
    MapTopic {pos, size, displayMode} ->
      case displayMode of
        Monad LabelOnly -> (topicExtent pos rectAcc, model)
        Monad Detail -> (detailTopicExtent mapItem.id mapId pos size rectAcc model, model)
        Container BlackBox -> (topicExtent pos rectAcc, model)
        Container WhiteBox ->
          let
            (rect, model_) = calcMapRect (mapItem.id :: pathToParent) model -- recursion
          in
          (mapExtent pos rect rectAcc, model_)
        Container Unboxed -> (topicExtent pos rectAcc, model)
    MapAssoc _ -> (rectAcc, model)


{-| Stores the map's "newRect" and, based on its change, calculates and stores the map's "pos"
adjustmennt ("delta")
-}
storeMapGeometry : MapPath -> Rectangle -> Rectangle -> Model -> Model
storeMapGeometry mapPath newRect oldRect model =
  case mapPath of
    mapId :: parentMapId :: _ ->
      let
        (isDragInProgress, isOnDragPath, isMapInDragPath) =
          case model.mouse.dragState of
            Drag DragTopic _ dragPath _ _ _ ->
              (True
              , (dragPath |> List.drop (List.length dragPath - List.length mapPath)) == mapPath
              , List.member mapId dragPath
              )
            _ -> (False, False, False)
      in
      if isDragInProgress then
        if isOnDragPath then
          model
          |> storeMapRect mapId newRect
          |> adjustMapPos mapId parentMapId newRect oldRect
          -- if maps are revealed more than once only those within the drag-path
          -- get the position adjustment, the other map's positions remain stable
        else
          if isMapInDragPath then
            model
            -- do nothing, postpone map's geometry update until reaching drag-path,
            -- otherwise, when reaching drag-path, the map's rect would be updated
            -- already and position adjustment will calculate 0
          else
            model |> storeMapRect mapId newRect
      else
        model |> storeMapRect mapId newRect
    [_] -> model -- do nothing, for the fullscreen map there is no geometry update
    [] -> logError "storeMapGeometry" "mapPath is empty!" model


storeMapRect : MapId -> Rectangle -> Model -> Model
storeMapRect mapId newRect model =
  model |> updateMapRect mapId (\rect -> newRect)


adjustMapPos : MapId -> MapId -> Rectangle -> Rectangle -> Model -> Model
adjustMapPos mapId parentMapId newRect oldRect model =
  model |> setTopicPosByDelta mapId parentMapId
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


detailTopicExtent : Id -> MapId -> Point -> Size -> Rectangle -> Model -> Rectangle
detailTopicExtent topicId mapId pos size rectAcc model =
  let
    textWidth =
      if model.editState == ItemEdit topicId mapId then
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
