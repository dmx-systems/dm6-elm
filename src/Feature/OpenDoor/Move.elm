module Feature.OpenDoor.Move exposing (move)

import Dict exposing (Dict)
import Model exposing (..)


{-| General move: relocate a topic from a container’s inner map
to a given target map. If the topic does not exist inside the
container’s inner map, this is a no-op.
-}
move :
    { containerId : MapId
    , topicId : Id
    , targetMapId : MapId
    }
    -> Model
    -> Model
move { containerId, topicId, targetMapId } model0 =
    case Dict.get containerId model0.maps of
        Nothing ->
            model0

        Just containerMap ->
            case Dict.get topicId containerMap.items of
                Nothing ->
                    model0

                Just itemInContainer ->
                    -- 1) remove from container’s inner map
                    let
                        model1 : Model
                        model1 =
                            { model0
                                | maps =
                                    Dict.insert
                                        containerId
                                        { containerMap
                                            | items = Dict.remove topicId containerMap.items
                                        }
                                        model0.maps
                            }
                    in
                    -- 2) insert/replace on the target map (visible; not pinned)
                    case Dict.get targetMapId model1.maps of
                        Nothing ->
                            model1

                        Just targetMap ->
                            let
                                movedItem : MapItem
                                movedItem =
                                    { id = topicId
                                    , hidden = False
                                    , pinned = False
                                    , props = itemInContainer.props
                                    , parentAssocId = -1 -- TODO: set when wiring composition assocs
                                    }
                            in
                            { model1
                                | maps =
                                    Dict.insert
                                        targetMapId
                                        { targetMap
                                            | items =
                                                Dict.insert topicId movedItem targetMap.items
                                        }
                                        model1.maps
                            }
