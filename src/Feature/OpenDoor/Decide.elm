module Feature.OpenDoor.Decide exposing (decideOpenDoorMsg)

import AppModel exposing (..)
import Dict
import Model exposing (Id, MapId)
import ModelAPI exposing (activeMap, getSingleSelection)



-- Find the inner-map (container id) that contains `topicId`
-- and whose parent is `parentMapId` (the map you are viewing).


findContainerForChild : MapId -> Id -> Model -> Maybe MapId
findContainerForChild parentMapId topicId model =
    model.maps
        |> Dict.values
        |> List.filter (\m -> m.parentMapId == parentMapId)
        |> List.filter (\m -> Dict.member topicId m.items)
        |> List.head
        |> Maybe.map .id


decideOpenDoorMsg : Model -> Maybe Msg
decideOpenDoorMsg model =
    case getSingleSelection model of
        Nothing ->
            Nothing

        Just ( topicId, selectionMapId ) ->
            let
                activeId =
                    activeMap model
            in
            if selectionMapId == activeId then
                -- Fullscreen / inner-map case: we are *inside* the container.
                -- Enable if the inner map has a parent, and move from inner -> parent.
                case Dict.get activeId model.maps of
                    Just m ->
                        if m.parentMapId /= -1 then
                            Just (MoveTopicToParentMap activeId topicId)

                        else
                            Nothing

                    Nothing ->
                        Nothing

            else
                -- WhiteBox case: selection is on the parent; find the container that owns this topic.
                findContainerForChild activeId topicId model
                    |> Maybe.map (\containerId -> MoveTopicToParentMap containerId topicId)
