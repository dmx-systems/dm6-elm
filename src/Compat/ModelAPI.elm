module Compat.ModelAPI exposing
    ( addItemToMapDefault
    , createTopic
    , defaultModel
    , ensureMap
    , getMapItemById
    , isItemInMap
    , isMapTopic
    )

import AppModel exposing (Model)
import Config exposing (..)
import Dict
import Json.Encode as E
import Main
import Model exposing (..)
import ModelAPI



-- default model via Main.init null


defaultModel =
    Tuple.first (Main.init E.null)



-- Re-exports with correct types


createTopic : String -> Maybe IconName -> Model -> ( Model, Id )
createTopic =
    ModelAPI.createTopic


getMapItemById : Id -> MapId -> Maps -> Maybe MapItem
getMapItemById =
    ModelAPI.getMapItemById



-- Stable predicate


isMapTopic : MapItem -> Bool
isMapTopic =
    ModelAPI.isMapTopic



-- New: used by tests


isItemInMap : Id -> MapId -> Model -> Bool
isItemInMap id mapId model =
    case getMapItemById id mapId model.maps of
        Just _ ->
            True

        Nothing ->
            False



-- Helper used by tests to avoid raw constructors


{-| Ensure a map exists in `model.maps`.

root is id 0; other maps default to being children of 0 by containment

-}
ensureMap : MapId -> Model -> Model
ensureMap mapId m =
    if Dict.member mapId m.maps then
        m

    else
        { m
            | maps =
                Dict.insert
                    mapId
                    (Model.Map mapId (Model.Rectangle 0 0 0 0) Dict.empty)
                    m.maps
        }


addItemToMapDefault : Id -> MapId -> Model -> Model
addItemToMapDefault id mapId model =
    let
        -- make sure both home and target maps exist
        base =
            model
                |> ensureMap 0
                |> ensureMap mapId

        tp : TopicProps
        tp =
            ModelAPI.defaultProps id topicSize base
    in
    -- addItemToMap needs MapProps → MapTopic tp
    ModelAPI.addItemToMap id (MapTopic tp) mapId base
