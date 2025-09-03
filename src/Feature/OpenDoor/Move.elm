module Feature.OpenDoor.Move exposing (move)

-- needs `moveTopicToMap` exposed

import AppModel exposing (Model)
import Dict
import Main
import Model
    exposing
        ( Id
        , MapId
        , MapProps(..)
        , Point
        )
import ModelAPI


move :
    { containerId : MapId
    , topicId : Id
    , targetMapId : MapId
    }
    -> Model
    -> Model
move { containerId, topicId, targetMapId } model0 =
    -- no-op guards: same container/child, missing container, or child not in container
    if containerId == topicId then
        model0

    else
        case Dict.get containerId model0.maps of
            Nothing ->
                model0

            Just containerMap ->
                case Dict.get topicId containerMap.items of
                    Nothing ->
                        model0

                    Just _ ->
                        let
                            -- original position of the topic inside its current container
                            origPos : Point
                            origPos =
                                case ModelAPI.getTopicProps topicId containerId model0.maps of
                                    Just tp ->
                                        tp.pos

                                    Nothing ->
                                        Point 0 0

                            -- try to place the dropped topic near the container's position
                            containerPosOnTarget : Maybe Point
                            containerPosOnTarget =
                                ModelAPI.getTopicProps containerId targetMapId model0.maps
                                    |> Maybe.map .pos

                            defaultDropPos : Point
                            defaultDropPos =
                                Point 40 40

                            newPos : Point
                            newPos =
                                case containerPosOnTarget of
                                    Just { x, y } ->
                                        Point (x + 24) (y + 24)

                                    Nothing ->
                                        defaultDropPos

                            -- active view path when dropping; keep it simple: focus the target map
                            targetMapPath =
                                [ targetMapId ]
                        in
                        -- Delegate to upstream move logic
                        Main.moveTopicToMap
                            topicId
                            containerId
                            origPos
                            targetMapId
                            targetMapPath
                            newPos
                            model0
