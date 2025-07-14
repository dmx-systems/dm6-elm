module MapAutoSize exposing (autoSize)

import Model exposing (..)
import Style exposing (..)

import Dict



autoSize : MapId -> Maps -> Maps
autoSize mapId maps =
  updateMapGeometry mapId 0 maps |> Tuple.second


-- called indirect recursive
updateMapGeometry : MapId -> Int -> Maps -> (Rectangle, Maps)
updateMapGeometry mapId level maps =
  -- 1) calculate and store the map's "rect" and, based on its change,
  -- 2) calculate and store the map's "pos" adjustmennt ("delta")
  let
    (rect, maps_) = calcMapRect mapId level maps
    maps__ =
      if level == 0 then
        maps_
      else
        case getMap mapId maps_ of
          Just map ->
            let
              delta = Point
                (rect.x1 - map.rect.x1)
                (rect.y1 - map.rect.y1)
            in
            updateMapRect mapId rect maps_
              |> updateTopicPos mapId map.parentMapId delta
          Nothing -> maps_
  in
  (rect, maps__)


updateMapRect : MapId -> Rectangle -> Maps -> Maps
updateMapRect mapId rect maps =
  updateMaps
    mapId
    (\map -> { map | rect = rect })
    maps


calcMapRect : MapId -> Int -> Maps -> (Rectangle, Maps)
calcMapRect mapId level maps =
  case getMap mapId maps of
    Just map ->
      (map.items |> Dict.values |> List.foldr
        (\viewItem (rect, maps_) ->
          if not viewItem.hidden then
            calcItemSize viewItem rect level maps_
          else
            (rect, maps_)
        )
        (Rectangle 5000 5000 -5000 -5000, maps) -- x-min y-min x-max y-max
      )
    Nothing -> (Rectangle 0 0 0 0, maps)


calcItemSize : ViewItem -> Rectangle -> Int -> Maps -> (Rectangle, Maps)
calcItemSize viewItem rect level maps =
  case viewItem.viewProps of
    ViewTopic {pos, displayMode} ->
      case displayMode of
        Just BlackBox -> (topicExtent pos rect, maps)
        Just WhiteBox ->
          let
            (rect_, maps_) = updateMapGeometry viewItem.id (level + 1) maps
          in
          (mapExtent pos rect_ rect, maps_)
        Just Unboxed -> (topicExtent pos rect, maps)
        Nothing -> (topicExtent pos rect, maps)
    ViewAssoc _ -> (rect, maps)


topicExtent : Point -> Rectangle -> Rectangle
topicExtent pos rectAcc =
  let
    tw2 = topicSize.w / 2
    th2 = topicSize.h / 2
  in
  Rectangle
    (min rectAcc.x1 (pos.x - tw2 - whiteBoxPadding))
    (min rectAcc.y1 (pos.y - th2 - whiteBoxPadding))
    (max rectAcc.x2 (pos.x + tw2 + whiteBoxPadding + 2 * topicBorderWidth))
    (max rectAcc.y2 (pos.y + th2 + whiteBoxPadding + 2 * topicBorderWidth))


mapExtent : Point -> Rectangle -> Rectangle -> Rectangle
mapExtent pos rect rectAcc =
  let
    tw2 = topicSize.w / 2
    th2 = topicSize.h / 2
    mw = rect.x2 - rect.x1
    mh = rect.y2 - rect.y1
  in
  Rectangle
    (min rectAcc.x1 (pos.x - tw2 - whiteBoxPadding))
    (min rectAcc.y1 (pos.y - th2 - whiteBoxPadding))
    (max rectAcc.x2 (pos.x - tw2 + mw + whiteBoxPadding))
    (max rectAcc.y2 (pos.y + th2 + mh + whiteBoxPadding))
