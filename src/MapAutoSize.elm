module MapAutoSize exposing (autoSize)

import Model exposing (..)
import Config exposing (..)

import Dict



-- UPDATE


autoSize : Model -> Model
autoSize model =
  calcMapRect model.activeMap model |> Tuple.second


-- called indirect recursive
-- 1) calculate and store the map's "rect" and, based on its change,
-- 2) calculate and store the map's "pos" adjustmennt ("delta")
calcMapRect : MapId -> Model -> (Rectangle, Model)
calcMapRect mapId model =
  case getMap mapId model.maps of
    Just map ->
      let
        (rect, model_) =
          (map.items |> Dict.values |> List.filter isVisible |> List.foldr
            (\viewItem (rectAcc, modelAcc) ->
              calcItemSize viewItem mapId rectAcc modelAcc
            )
            (Rectangle 5000 5000 -5000 -5000, model) -- x-min y-min x-max y-max
          )
        newRect = Rectangle
          (rect.x1 - whiteBoxPadding)
          (rect.y1 - whiteBoxPadding)
          (rect.x2 + whiteBoxPadding)
          (rect.y2 + whiteBoxPadding)
      in
      storeMapRect mapId newRect map.rect map.parentMapId model_
    Nothing -> (Rectangle 0 0 0 0, model)


storeMapRect : MapId -> Rectangle -> Rectangle -> MapId -> Model -> (Rectangle, Model)
storeMapRect mapId newRect oldRect parentMapId model =
  if mapId == model.activeMap then
    ( newRect, model )
  else
    ( newRect
    , { model | maps = model.maps
        |> updateMaps mapId (\map -> { map | rect = newRect })
        |> setTopicPosByDelta mapId parentMapId
          ( Point
            ( newRect.x1 - oldRect.x1 )
            ( newRect.y1 - oldRect.y1 )
          )
      }
    )


calcItemSize : ViewItem -> MapId -> Rectangle -> Model -> (Rectangle, Model)
calcItemSize viewItem mapId rectAcc model =
  case viewItem.viewProps of
    ViewTopic {pos, size, displayMode} ->
      case displayMode of
        Monad LabelOnly -> (topicExtent pos rectAcc, model)
        Monad Detail -> (detailTopicExtent viewItem.id mapId pos size rectAcc model, model)
        Container BlackBox -> (topicExtent pos rectAcc, model)
        Container WhiteBox ->
          let
            (rect, model_) = calcMapRect viewItem.id model
          in
          (mapExtent pos rect rectAcc, model_)
        Container Unboxed -> (topicExtent pos rectAcc, model)
    ViewAssoc _ -> (rectAcc, model)


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
