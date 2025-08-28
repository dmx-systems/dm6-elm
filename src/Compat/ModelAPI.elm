module Compat.ModelAPI exposing
    ( addItemToMapDefault
    , createTopic
    , defaultModel
    , getMapItemById
    , isItemInMap
    , isMapTopic
    )

import AppModel exposing (Model)
import Config exposing (..)
import Json.Encode as E
import Main as AppMain
import Model exposing (IconName, Id, MapId, MapItem, MapProps(..), Maps, TopicProps)
import ModelAPI



-- default model via Main.init null


defaultModel =
    Tuple.first (AppMain.init E.null)



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


addItemToMapDefault : Id -> MapId -> Model -> Model
addItemToMapDefault id mapId model =
    let
        tp : TopicProps
        tp =
            ModelAPI.defaultProps id topicSize model
    in
    -- addItemToMap needs MapProps → MapTopic tp
    ModelAPI.addItemToMap id (MapTopic tp) mapId model
