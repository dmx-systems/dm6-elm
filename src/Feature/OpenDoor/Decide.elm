module Feature.OpenDoor.Decide exposing (decideOpenDoorMsg)

import AppModel exposing (Model, Msg(..))
import Dict
import Log exposing (info)
import Model exposing (..)
import ModelAPI exposing (activeMap, getSingleSelection)


{-| Decide what the Cross button should do.

    - Containers (topicId is also a map id): navigate (enter/exit).
    - Monads: move across the nearest boundary (parent ↔ inner).
      If no owning container exists on the parent, fall back to a no-op move,
      so the button still dispatches a predictable message.

    Every branch emits a Log.info for traceability.

-}
decideOpenDoorMsg : Model -> Maybe Msg
decideOpenDoorMsg model =
    case getSingleSelection model of
        Nothing ->
            let
                _ =
                    info "Cross" { reason = "no selection" }
            in
            Nothing

        Just ( topicId, selectionMapId ) ->
            let
                here : MapId
                here =
                    activeMap model

                origin : Point
                origin =
                    Point 0 0

                -- A topic is a container iff there is a map with the same id
                isContainer : Bool
                isContainer =
                    Dict.member topicId model.maps
            in
            if isContainer then
                -- Containers never move; cross by navigation (enter/exit).
                if selectionMapId == here then
                    let
                        _ =
                            info "Cross (enter container)" { container = topicId }
                    in
                    Just (Nav Fullscreen)

                else
                    let
                        _ =
                            info "Cross (exit container)" { container = selectionMapId }
                    in
                    Just (Nav Back)

            else if selectionMapId == here then
                -- Monad on parent → move into its owning container if present.
                case findContainerForChild here topicId model of
                    Just containerId ->
                        let
                            _ =
                                info "Cross (parent→inner)" { topicId = topicId, src = here, dst = containerId }
                        in
                        Just (MoveTopicToMap topicId here origin topicId containerId origin)

                    Nothing ->
                        -- no container on parent; do a no-op move for consistency
                        let
                            _ =
                                info "Cross (no-op)" { reason = "no owning container on parent", topicId = topicId, parent = here }
                        in
                        Just (MoveTopicToMap topicId here origin topicId here origin)

            else
                -- Monad inside an inner map → move to that inner map's parent.
                case findParentOf selectionMapId model of
                    Just parentId ->
                        let
                            _ =
                                info "Cross (inner→parent)" { topicId = topicId, src = selectionMapId, dst = parentId }
                        in
                        Just (MoveTopicToMap topicId selectionMapId origin topicId parentId origin)

                    Nothing ->
                        let
                            _ =
                                info "Cross disabled" { reason = "inner map has no parent", inner = selectionMapId }
                        in
                        Nothing


{-| Does `parentId` contain the inner map `childId`?
-}
isChildOf : MapId -> MapId -> Model -> Bool
isChildOf childId parentId model =
    case Dict.get parentId model.maps of
        Just parentMap ->
            Dict.member childId parentMap.items

        Nothing ->
            False


{-| Find the parent map of an inner map by containment (first match).
-}
findParentOf : MapId -> Model -> Maybe MapId
findParentOf childId model =
    model.maps
        |> Dict.values
        |> List.filter (\pm -> Dict.member childId pm.items)
        |> List.head
        |> Maybe.map .id


{-| Find the container (its inner-map id equals the container topic id) under `parentId`
that contains `topicId` in its inner map.
-}
findContainerForChild : MapId -> Id -> Model -> Maybe MapId
findContainerForChild parentId topicId model =
    model.maps
        |> Dict.values
        |> List.filter (\m -> Dict.member topicId m.items)
        -- inner map contains the topic
        |> List.filter (\m -> isChildOf m.id parentId model)
        -- and that inner map belongs to the parent view
        |> List.head
        |> Maybe.map .id
