module MapAutoSize exposing (autoSize)

import AppModel exposing (..)
import Config exposing (..)
import Model exposing (..)
import ModelAPI exposing (..)
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
    mapId = getMapId mapPath
  in
  case getMap mapId model.maps of
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
          (rect.x1 - whiteBoxPadding)
          (rect.y1 - whiteBoxPadding)
          (rect.x2 + whiteBoxPadding)
          (rect.y2 + whiteBoxPadding)
      in
      ( newRect
      , storeMapGeometry mapPath newRect map.rect model_
      )
    Nothing -> (Rectangle 0 0 0 0, model)


calcItemSize : MapItem -> MapPath -> Rectangle -> Model -> (Rectangle, Model)
calcItemSize mapItem pathToParent rectAcc model =
  let
    mapId = getMapId pathToParent
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
      model
      |> updateMapRect mapId (\rect -> newRect)
      |> setTopicPosByDelta mapId parentMapId
        (Point
          (newRect.x1 - oldRect.x1)
          (newRect.y1 - oldRect.y1)
        )
    [_] -> model
    [] -> logError "storeMapGeometry" "mapPath is empty!" model


topicExtent : Point -> Rectangle -> Rectangle
topicExtent pos rectAcc =
  Rectangle
    (min rectAcc.x1 (pos.x - topicW2))
    (min rectAcc.y1 (pos.y - topicH2))
    (max rectAcc.x2 (pos.x + topicW2 + 2 * topicBorderWidth))
    (max rectAcc.y2 (pos.y + topicH2 + 2 * topicBorderWidth))


detailTopicExtent : Id -> MapId -> Point -> Size -> Rectangle -> Model -> Rectangle
detailTopicExtent topicId mapId pos size rectAcc model =
  let
    textWidth =
      if model.editState == ItemEdit topicId mapId then
        topicDetailMaxWidth
      else
        size.w
  in
  Rectangle
    (min rectAcc.x1 (pos.x - topicW2))
    (min rectAcc.y1 (pos.y - topicH2))
    (max rectAcc.x2 (pos.x - topicW2 + textWidth + topicSize.h + 2 * topicBorderWidth))
    (max rectAcc.y2 (pos.y - topicH2 + size.h + 2 * topicBorderWidth))


mapExtent : Point -> Rectangle -> Rectangle -> Rectangle
mapExtent pos rect rectAcc =
  let
    mapWidth = rect.x2 - rect.x1
    mapHeight = rect.y2 - rect.y1
  in
  Rectangle
    (min rectAcc.x1 (pos.x - topicW2))
    (min rectAcc.y1 (pos.y - topicH2))
    (max rectAcc.x2 (pos.x - topicW2 + mapWidth))
    (max rectAcc.y2 (pos.y + topicH2 + mapHeight))
