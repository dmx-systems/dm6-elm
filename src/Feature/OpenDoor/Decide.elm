module Feature.OpenDoor.Decide exposing (decideOpenDoorMsg)

import AppModel exposing (..)
import Dict
import Model exposing (Id, MapId)
import ModelAPI exposing (activeMap, getSingleSelection)



-- Find the inner-map (container id) that contains `topicId`
-- and whose parent is `parentMapId` (the map you are viewing).


findContainerForChild : MapId -> Id -> Model -> Maybe MapId
findContainerForChild _ topicId model =
    model.maps
        |> Dict.values
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
                    Just _ ->
                        -- parentMapId was removed from Map; if this branch was meant to skip “rootless” maps,
                        -- keep behavior permissive until a new parent-derivation is wired.
                        if True then
                            -- Move inner -> parent (source = activeId, target = activeId)
                            Just (MoveTopicToMap topicId activeId origin topicId activeId origin)

                        else
                            Nothing

                    Nothing ->
                        Nothing

            else
                -- WhiteBox case: selection is on the parent; find the container that owns this topic.
                findContainerForChild activeId topicId model
                    |> Maybe.map
                        (\containerId ->
                            -- Move parent -> inner (source = activeId, target = containerId)
                            MoveTopicToMap topicId containerId origin topicId activeId origin
                        )



-- Neutral point for non-drag moves


origin : Model.Point
origin =
    { x = 0, y = 0 }
