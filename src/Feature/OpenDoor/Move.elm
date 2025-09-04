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
                            -- derive parent (target for "move out")
                            parentPath : MapPath
                            parentPath =
                                parentPathOf containerId model0

                            parentId : MapId
                            parentId =
                                List.head parentPath |> Maybe.withDefault 0

                            -- original position of the topic inside its current container
                            origPos : Point
                            origPos =
                                case ModelAPI.getTopicProps topicId containerId model0.maps of
                                    Just tp ->
                                        tp.pos

                                    Nothing ->
                                        Point 0 0

                            -- try to place the dropped topic near the container's position on its parent map
                            containerPosOnParent : Maybe Point
                            containerPosOnParent =
                                ModelAPI.getTopicProps containerId parentId model0.maps
                                    |> Maybe.map .pos

                            defaultDropPos : Point
                            defaultDropPos =
                                Point 40 40

                            newPos : Point
                            newPos =
                                case containerPosOnParent of
                                    Just { x, y } ->
                                        Point (x + 24) (y + 24)

                                    Nothing ->
                                        defaultDropPos
                        in
                        -- Cross the boundary: move topic from container into its parent map
                        Main.moveTopicToMap topicId containerId origPos parentId parentPath newPos model0
