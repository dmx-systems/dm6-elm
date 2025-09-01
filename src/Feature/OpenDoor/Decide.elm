module Feature.OpenDoor.Decide exposing (decideOpenDoorMsg)

import AppModel exposing (Model, Msg(..))
import Dict
import Log exposing (info)
import Model exposing (..)
import ModelAPI exposing (activeMap, getDisplayMode, getSingleSelection)


{-| Decide what the Cross button should do.

    - Monads: normally move across the nearest container boundary (parent ↔ inner).
      If no container exists on the parent, fall back to a no-op move (still
      enables the button for UI consistency).
    - Containers: cross by navigation (enter/exit), do not attempt to move the
      container node itself.

    Every branch logs a decision with `Log.info`.

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

                -- Robust container check: a topic is a container iff there is a map with the same id
                isContainer : Bool
                isContainer =
                    Dict.member topicId model.maps
            in
            if isContainer then
                -- Containers never move; cross by navigation (enter/exit)
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

            else
            -- Monads move across the nearest boundary
            if
                selectionMapId == here
            then
                -- Parent → inner. If no owning container exists on parent, fall back to a no-op move
                case findContainerForChild here topicId model of
                    Just containerId ->
                        let
                            _ =
                                info "Cross (parent→inner)" { topicId = topicId, src = here, dst = containerId }
                        in
                        Just (MoveTopicToMap topicId here origin topicId containerId origin)

                    Nothing ->
                        -- Fallback expected by tests: enable button and dispatch a no-op move
                        let
                            _ =
                                info "Cross (no-op)" { reason = "no owning container on parent", topicId = topicId, parent = here }
                        in
                        Just (MoveTopicToMap topicId here origin topicId here origin)

            else
                -- Inner → parent
                case Dict.get selectionMapId model.maps of
                    Just m ->
                        if m.parentMapId /= -1 then
                            let
                                _ =
                                    info "Cross (inner→parent)" { topicId = topicId, src = selectionMapId, dst = m.parentMapId }
                            in
                            Just (MoveTopicToMap topicId selectionMapId origin topicId m.parentMapId origin)

                        else
                            let
                                _ =
                                    info "Cross disabled" { reason = "inner map has no parent", inner = selectionMapId }
                            in
                            Nothing

                    Nothing ->
                        let
                            _ =
                                info "Cross disabled" { reason = "unknown inner map", inner = selectionMapId }
                        in
                        Nothing


{-| Find the container (its inner-map id equals the container topic id) under `parentMapId`
that contains `topicId` in its inner map.
-}
findContainerForChild : MapId -> Id -> Model -> Maybe MapId
findContainerForChild parentMapId topicId model =
    model.maps
        |> Dict.values
        |> List.filter (\m -> m.parentMapId == parentMapId)
        |> List.filter (\m -> Dict.member topicId m.items)
        |> List.head
        |> Maybe.map .id
