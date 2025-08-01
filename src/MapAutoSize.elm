module MapAutoSize exposing (autoSize)

import Model exposing (..)
import Config exposing (..)

import Dict



-- UPDATE


autoSize : Model -> Model
autoSize model =
  updateMapGeometry model.activeMap 0 model |> Tuple.second


-- called indirect recursive
updateMapGeometry : MapId -> Int -> Model -> (Rectangle, Model)
updateMapGeometry mapId level model =
  -- 1) calculate and store the map's "rect" and, based on its change,
  -- 2) calculate and store the map's "pos" adjustmennt ("delta")
  let
    (rect, model_) = calcMapRect mapId level model
    model__ =
      if level == 0 then
        model_
      else
        case getMap mapId model_.maps of
          Just map ->
            let
              delta = Point
                (rect.x1 - map.rect.x1)
                (rect.y1 - map.rect.y1)
            in
            { model_ | maps =
              updateMapRect mapId rect model_.maps
              |> setTopicPosByDelta mapId map.parentMapId delta
            }
          Nothing -> model_
  in
  (rect, model__)


updateMapRect : MapId -> Rectangle -> Maps -> Maps
updateMapRect mapId rect maps =
  updateMaps
    mapId
    (\map -> { map | rect = rect })
    maps


calcMapRect : MapId -> Int -> Model -> (Rectangle, Model)
calcMapRect mapId level model =
  case getMap mapId model.maps of
    Just map ->
      (map.items |> Dict.values |> List.filter isVisible |> List.foldr
        (\viewItem (rect, model_) ->
          calcItemSize viewItem mapId rect level model_
        )
        (Rectangle 5000 5000 -5000 -5000, model) -- x-min y-min x-max y-max
      )
    Nothing -> (Rectangle 0 0 0 0, model)


calcItemSize : ViewItem -> MapId -> Rectangle -> Int -> Model -> (Rectangle, Model)
calcItemSize viewItem mapId rect level model =
  case viewItem.viewProps of
    ViewTopic {pos, size, displayMode} ->
      case displayMode of
        Monad LabelOnly -> (topicExtent pos rect, model)
        Monad Detail -> (detailTopicExtent viewItem.id mapId pos size rect model, model)
        Container BlackBox -> (topicExtent pos rect, model)
        Container WhiteBox ->
          let
            (rect_, model_) = updateMapGeometry viewItem.id (level + 1) model
          in
          (mapExtent pos rect_ rect, model_)
        Container Unboxed -> (topicExtent pos rect, model)
    ViewAssoc _ -> (rect, model)


topicExtent : Point -> Rectangle -> Rectangle
topicExtent pos rectAcc =
  Rectangle
    (min rectAcc.x1 (pos.x - topicW2 - whiteBoxPadding))
    (min rectAcc.y1 (pos.y - topicH2 - whiteBoxPadding))
    (max rectAcc.x2 (pos.x + topicW2 + whiteBoxPadding + 2 * topicBorderWidth))
    (max rectAcc.y2 (pos.y + topicH2 + whiteBoxPadding + 2 * topicBorderWidth))


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
    (min rectAcc.x1 (pos.x - topicW2 - whiteBoxPadding))
    (min rectAcc.y1 (pos.y - topicH2 - whiteBoxPadding))
    (max rectAcc.x2 (pos.x - topicW2 + textWidth + topicSize.h + whiteBoxPadding + 2 * topicBorderWidth))
    (max rectAcc.y2 (pos.y - topicH2 + size.h + whiteBoxPadding + 2 * topicBorderWidth))


mapExtent : Point -> Rectangle -> Rectangle -> Rectangle
mapExtent pos rect rectAcc =
  let
    mw = rect.x2 - rect.x1
    mh = rect.y2 - rect.y1
  in
  Rectangle
    (min rectAcc.x1 (pos.x - topicW2 - whiteBoxPadding))
    (min rectAcc.y1 (pos.y - topicH2 - whiteBoxPadding))
    (max rectAcc.x2 (pos.x - topicW2 + mw + whiteBoxPadding))
    (max rectAcc.y2 (pos.y + topicH2 + mh + whiteBoxPadding))
