module Feature.OpenDoor.Copy exposing
    ( copyFromContainer
    , copyToMap
    )

import AppModel exposing (Model)
import Dict
import Model exposing (Id, MapId)


{-| Copy a topic from `sourceMapId` to `targetMapId`.

Semantics on destination:
• hidden = False
• pinned = False
• parentAssocId = -1

Idempotent on the source: does not remove or alter the item in the source map.
If either map or the item is missing, this is a no-op.

-}
copyToMap :
    { sourceMapId : MapId
    , topicId : Id
    , targetMapId : MapId
    }
    -> Model
    -> Model
copyToMap { sourceMapId, topicId, targetMapId } model0 =
    case ( Dict.get sourceMapId model0.maps, Dict.get targetMapId model0.maps ) of
        ( Just srcMap, Just tgtMap ) ->
            case Dict.get topicId srcMap.items of
                Nothing ->
                    -- Topic not present in source map -> no-op
                    model0

                Just srcItem ->
                    let
                        copiedItem : MapItem
                        copiedItem =
                            { id = topicId
                            , hidden = False
                            , pinned = False
                            , props = srcItem.props
                            , parentAssocId = -1 -- wire this when creating/wiring composition assocs
                            }
                    in
                    { model0
                        | maps =
                            Dict.insert
                                targetMapId
                                { tgtMap
                                    | items = Dict.insert topicId copiedItem tgtMap.items
                                }
                                model0.maps
                    }

        _ ->
            -- Missing source or target map -> no-op
            model0


{-| Convenience: Copy a topic OUT OF a container's inner map into `targetMapId`.
This does not remove the topic from the container (pure copy).
-}
copyFromContainer :
    { containerId : MapId
    , topicId : Id
    , targetMapId : MapId
    }
    -> Model
    -> Model
copyFromContainer { containerId, topicId, targetMapId } =
    copyToMap
        { sourceMapId = containerId
        , topicId = topicId
        , targetMapId = targetMapId
        }
