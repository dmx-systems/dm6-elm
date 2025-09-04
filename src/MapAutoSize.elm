module MapAutoSize exposing (autoSize)

import AppModel exposing (..)
import Config exposing (..)
import Model exposing (..)
import ModelAPI exposing (..)
import Utils exposing (info, logError)

import Dict



-- UPDATE


autoSize : Model -> Model
autoSize model =
  let
    paths = findPaths model
    _ = info "autoSize" paths
  in
  paths |> List.foldl resizeMaps model -- left-fold resizes maps in selection path first


{-| Finds the paths of the maps that might need resizing, based on current selection. Maps
might occur in several paths, the "pos adjustment" though (see storeMapRect) must be applied
only to the selection path (where interaction took place). This is done by returning the
selection path first, the other map occurrences will calculate a zero adjustment then.
-}
findPaths : Model -> List MapPath
findPaths model =
  case getSingleSelection model of
    Just (_, selPath) ->
      case selPath of
        mapId :: _ :: _ ->
          let
            activeMapId = activeMap model
          in
          selPath :: -- put selection path at begin
            (findPaths_ activeMapId [ activeMapId ] mapId [] model
              |> List.filter ((/=) selPath)
            )
        [_] -> []
        [] -> logError "findPaths" "selPath is empty!" []
    Nothing -> []


{-| "itemId" is not necessarily a map Id -}
findPaths_ : Id -> MapPath -> MapId -> List MapPath -> Model -> List MapPath
findPaths_ itemId pathToItem searchMapId foundPaths model =
  case itemId == searchMapId of
    True -> pathToItem :: foundPaths
    False ->
      case getMapIfExists itemId model.maps of
        Just map ->
          map.items |> Dict.values |> List.filter isVisible |> List.foldr -- TODO: filter topics
            (\mapItem foundPathsAcc ->
              -- recursion
              findPaths_ mapItem.id (mapItem.id :: pathToItem) searchMapId foundPathsAcc model
            )
            foundPaths
        Nothing -> foundPaths


resizeMaps : MapPath -> Model -> Model
resizeMaps mapPath model =
  case mapPath of
    [ mapId ] -> model
    mapId :: parentMapId :: mapIds ->
      model
      |> resizeMap mapId parentMapId
      |> resizeMaps (parentMapId :: mapIds) -- recursion
    [] -> logError "resizeMaps" "mapPath is empty!" model


{-| Based on its content calculates the map's size (its "rect") and its position adjustment.
Nested maps are expected to be sized already. Returns the updated model.
-}
resizeMap : MapId -> MapId -> Model -> Model
resizeMap mapId parentMapId model =
  case getMap mapId model.maps of
    Just map ->
      let
        rect =
          (map.items |> Dict.values |> List.filter isVisible |> List.foldr
            (\mapItem rectAcc ->
              accumulateSize mapItem mapId rectAcc model
            )
            (Rectangle 5000 5000 -5000 -5000) -- x-min y-min x-max y-max
          )
        newRect = Rectangle
          (rect.x1 - whiteBoxPadding)
          (rect.y1 - whiteBoxPadding)
          (rect.x2 + whiteBoxPadding)
          (rect.y2 + whiteBoxPadding)
      in
      storeMapRect mapId newRect map.rect parentMapId model
    Nothing -> model -- error is already logged


accumulateSize : MapItem -> MapId -> Rectangle -> Model -> Rectangle
accumulateSize mapItem mapId rectAcc model =
  case mapItem.props of
    MapTopic {pos, size, displayMode} ->
      case displayMode of
        Monad LabelOnly -> topicExtent pos rectAcc
        Monad Detail -> detailTopicExtent mapItem.id mapId pos size rectAcc model
        Container BlackBox -> topicExtent pos rectAcc
        Container WhiteBox ->
          case getMap mapItem.id model.maps of
            Just map -> mapExtent pos map.rect rectAcc
            Nothing -> Rectangle 0 0 0 0 -- error is already logged
        Container Unboxed -> topicExtent pos rectAcc
    MapAssoc _ -> rectAcc


{-| Store the map's "newRect" and, based on its change, calculate and stores the map's "pos"
adjustmennt ("delta")
-}
storeMapRect : MapId -> Rectangle -> Rectangle -> MapId -> Model -> Model
storeMapRect mapId newRect oldRect parentMapId model =
  model
  |> updateMapRect mapId (\rect -> newRect)
  |> setTopicPosByDelta mapId parentMapId
    (Point
      (newRect.x1 - oldRect.x1)
      (newRect.y1 - oldRect.y1)
    )


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
