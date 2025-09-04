module MapAutoSize exposing (autoSize)

import AppModel exposing (..)
import Config exposing (..)
import Dict
import Model exposing (..)
import ModelAPI
    exposing
        ( getMap
        , getSingleSelection
        , isVisible
        , setTopicPosByDelta
        , updateMapRect
        )
import Utils exposing (logError)



-- UPDATE


autoSize : Model -> Model
autoSize model =
    case getSingleSelection model of
        Just ( _, mapPath ) ->
            autoSizeMap mapPath model

        Nothing ->
            model


autoSizeMap : MapPath -> Model -> Model
autoSizeMap mapPath model =
    case mapPath of
        [ _ ] ->
            model

        mapId :: parentMapId :: mapIds ->
            model
                |> calcMapRect mapId parentMapId
                |> autoSizeMap (parentMapId :: mapIds)

        -- recursion
        [] ->
            logError "autoSizeMap" "mapPath is empty!" model


{-| Calculates the map's "rect"
-}
calcMapRect : MapId -> MapId -> Model -> Model
calcMapRect mapId parentMapId model =
    case getMap mapId model.maps of
        Just map ->
            let
                rect =
                    (map.items
                        |> Dict.values
                        |> List.filter isVisible
                        |> List.foldr
                            (\mapItem rectAcc ->
                                accumulateSize mapItem mapId rectAcc model
                            )
                            (Rectangle 5000 5000 -5000 -5000)
                     -- x-min y-min x-max y-max
                    )

                newRect =
                    Rectangle
                        (rect.x1 - whiteBoxPadding)
                        (rect.y1 - whiteBoxPadding)
                        (rect.x2 + whiteBoxPadding)
                        (rect.y2 + whiteBoxPadding)
            in
            storeMapRect mapId newRect map.rect parentMapId model

        Nothing ->
            model


accumulateSize : MapItem -> MapId -> Rectangle -> Model -> Rectangle
accumulateSize mapItem mapId rectAcc model =
    case mapItem.props of
        MapTopic { pos, size, displayMode } ->
            case displayMode of
                Monad LabelOnly ->
                    topicExtent pos rectAcc

                Monad Detail ->
                    detailTopicExtent mapItem.id mapId pos size rectAcc model

                Container BlackBox ->
                    topicExtent pos rectAcc

                Container WhiteBox ->
                    case getMap mapItem.id model.maps of
                        Just map ->
                            mapExtent pos map.rect rectAcc

                        Nothing ->
                            Rectangle 0 0 0 0

                -- error is already logged
                Container Unboxed ->
                    topicExtent pos rectAcc

        MapAssoc _ ->
            rectAcc


{-| Store the map's "newRect" and, based on its change, calculate and stores the map's "pos"
adjustmennt ("delta")
-}
storeMapRect : MapId -> Rectangle -> Rectangle -> MapId -> Model -> Model
storeMapRect mapId newRect oldRect parentMapId model =
    model
        |> updateMapRect mapId (\_ -> newRect)
        |> setTopicPosByDelta mapId
            parentMapId
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
        mapWidth =
            rect.x2 - rect.x1

        mapHeight =
            rect.y2 - rect.y1
    in
    Rectangle
        (min rectAcc.x1 (pos.x - topicW2))
        (min rectAcc.y1 (pos.y - topicH2))
        (max rectAcc.x2 (pos.x - topicW2 + mapWidth))
        (max rectAcc.y2 (pos.y + topicH2 + mapHeight))
