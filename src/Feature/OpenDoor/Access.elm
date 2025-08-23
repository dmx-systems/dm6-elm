module Feature.OpenDoor.Access exposing
    ( getMap
    , getMapItem
    , insertMapItem
    , removeMapItem
    , updateMap
    )

import Dict exposing (Dict)
import Model exposing (..)


getMap : MapId -> Model -> Maybe Map
getMap mapId model =
    Dict.get mapId model.maps


getMapItem : MapId -> Id -> Model -> Maybe MapItem
getMapItem mapId itemId model =
    case getMap mapId model of
        Just m ->
            Dict.get itemId m.items

        Nothing ->
            Nothing


updateMap : MapId -> (Map -> Map) -> Model -> Model
updateMap mapId f model =
    case getMap mapId model of
        Just m ->
            { model | maps = Dict.insert mapId (f m) model.maps }

        Nothing ->
            model


removeMapItem : MapId -> Id -> Model -> Model
removeMapItem mapId itemId =
    updateMap mapId <|
        \m -> { m | items = Dict.remove itemId m.items }


insertMapItem : MapId -> MapItem -> Model -> Model
insertMapItem mapId item =
    updateMap mapId <|
        \m -> { m | items = Dict.insert item.id item m.items }
