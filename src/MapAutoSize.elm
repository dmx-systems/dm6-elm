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
                ((rect.x1 + rect.x2 - map.rect.x1 - map.rect.x2) / 2)
                ((rect.y1 + rect.y2 - map.rect.y1 - map.rect.y2) / 2)
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
        Just BlackBox -> (extent pos topicRect rect, maps)
        Just WhiteBox ->
          let
            (rect_, maps_) = updateMapGeometry viewItem.id (level + 1) maps
          in
          (extent pos rect_ rect, maps_)
        Just Unboxed -> (extent pos topicRect rect, maps)
        Nothing -> (extent pos topicRect rect, maps)
    ViewAssoc _ -> (rect, maps)


extent : Point -> Rectangle -> Rectangle -> Rectangle
extent pos rect rectAcc =
  let
    w2 = (rect.x2 - rect.x1) / 2
    h2 = (rect.y2 - rect.y1) / 2
  in
  Rectangle
    (min rectAcc.x1 (pos.x - w2 - whiteBoxPadding))
    (min rectAcc.y1 (pos.y - h2 - whiteBoxPadding))
    (max rectAcc.x2 (pos.x + w2 + whiteBoxPadding))
    (max rectAcc.y2 (pos.y + h2 + whiteBoxPadding))
