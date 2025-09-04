module Feature.OpenDoor.Move exposing (move)

import AppModel exposing (Model)
import Compat.ModelAPI as ModelAPI
import Dict
import Main
import Model exposing (Id, MapId, MapPath, Point)



-- Helper: parent map path of a container/topic (fallback to [0])


parentPathOf : Id -> Model -> MapPath
parentPathOf containerId model =
    model.maps
        |> Dict.toList
        |> List.filter (\( _, m ) -> Dict.member containerId m.items)
        |> List.head
        |> Maybe.map (\( mapId, _ ) -> [ mapId ])
        |> Maybe.withDefault [ 0 ]


move :
    { containerId : MapId
    , topicId : Id
    , targetMapId : MapId
    }
    -> Model
    -> Model
move { containerId, topicId, targetMapId } model0 =
    if containerId == topicId then
        model0

    else
        let
            -- where the container (topic) currently lives (the parent map path)
            parentPath : MapPath
            parentPath =
                parentPathOf containerId model0

            parentId : MapId
            parentId =
                List.head parentPath |> Maybe.withDefault 0
        in
        if targetMapId == parentId then
            -- OUT of container: move topic from container’s inner map → parent map
            let
                -- topic’s original position *inside* the container
                origPosInside : Point
                origPosInside =
                    case ModelAPI.getTopicProps topicId containerId model0.maps of
                        Just tp ->
                            tp.pos

                        Nothing ->
                            Point 0 0

                dropNearContainerOnParent : Point
                dropNearContainerOnParent =
                    case ModelAPI.getTopicProps containerId parentId model0.maps |> Maybe.map .pos of
                        Just { x, y } ->
                            Point (x + 24) (y + 24)

                        Nothing ->
                            Point 40 40
            in
            Main.moveTopicToMap topicId containerId origPosInside parentId parentPath dropNearContainerOnParent model0

        else if targetMapId == containerId then
            -- INTO container: move topic from parent map → container’s inner map
            let
                -- topic’s original position on the parent map
                origPosOnParent : Point
                origPosOnParent =
                    case ModelAPI.getTopicProps topicId parentId model0.maps of
                        Just tp ->
                            tp.pos

                        Nothing ->
                            Point 0 0

                defaultInsidePos : Point
                defaultInsidePos =
                    Point 24 24
            in
            Main.moveTopicToMap topicId parentId origPosOnParent containerId [ containerId ] defaultInsidePos model0

        else
            -- neither into nor out (defensive no-op)
            model0
