module Feature.OpenDoor.Move exposing (move)

import Config exposing (topicSize, whiteBoxPadding)
import Dict exposing (Dict)
import Model exposing (..)


{-| General move: relocate a topic from a container’s inner map
to a given target map. If the topic does not exist inside the
container’s inner map, this is a no-op.

Semantics:

  - Remove the topic from the container’s inner map.
  - On the target (parent) map:
      - If a view of the topic already exists (e.g., WhiteBox), reuse it
        (keep its position and parentAssocId), and ensure it is visible / not pinned.
      - Otherwise, create a fresh MapItem:
          - visible, not pinned
          - if it’s a topic, drop it near the container’s position on the target map,
            or at a small default offset if the container is not visible there
          - parentAssocId = -1 (no composition wired on target yet)

Additionally:

  - If removing the topic empties the inner map, keep a minimal non-zero
    rectangle so the white box does not visually collapse.

-}
move :
    { containerId : MapId
    , topicId : Id
    , targetMapId : MapId
    }
    -> Model
    -> Model
move { containerId, topicId, targetMapId } model0 =
    if containerId == topicId then
        -- Don’t move the container itself; no-op
        model0

    else
        case Dict.get containerId model0.maps of
            Nothing ->
                model0

            Just containerMap ->
                case Dict.get topicId containerMap.items of
                    Nothing ->
                        -- Topic is not inside the container's inner map -> no-op
                        model0

                    Just itemInContainer ->
                        -- 1) Remove from container’s inner map
                        let
                            newItemsInContainer : Dict Id MapItem
                            newItemsInContainer =
                                Dict.remove topicId containerMap.items

                            -- Keep a minimal rect when inner map becomes empty,
                            -- so the container’s white box is still visible.
                            adjustedContainerRect : Rectangle
                            adjustedContainerRect =
                                if Dict.isEmpty newItemsInContainer then
                                    Rectangle
                                        0
                                        0
                                        (topicSize.w + 2 * whiteBoxPadding)
                                        (topicSize.h + 2 * whiteBoxPadding)

                                else
                                    containerMap.rect

                            model1 : Model
                            model1 =
                                { model0
                                    | maps =
                                        Dict.insert
                                            containerId
                                            { containerMap
                                                | items = newItemsInContainer
                                                , rect = adjustedContainerRect
                                            }
                                            model0.maps
                                }
                        in
                        -- 2) Insert/replace on the target map
                        case Dict.get targetMapId model1.maps of
                            Nothing ->
                                -- target map not found -> keep removal only
                                model1

                            Just targetMap ->
                                let
                                    -- If the topic already exists on the target (WhiteBox case),
                                    -- reuse it to keep stable position and parentAssocId.
                                    existingOnTarget : Maybe MapItem
                                    existingOnTarget =
                                        Dict.get topicId targetMap.items

                                    -- Try to place a new topic near the container’s view on target,
                                    -- if that view exists. Otherwise, use a small default offset.
                                    containerPosOnTarget : Maybe Point
                                    containerPosOnTarget =
                                        case Dict.get containerId targetMap.items of
                                            Just containerViewItem ->
                                                case containerViewItem.props of
                                                    MapTopic tp ->
                                                        Just tp.pos

                                                    _ ->
                                                        Nothing

                                            Nothing ->
                                                Nothing

                                    defaultDropPos : Point
                                    defaultDropPos =
                                        Point 40 40

                                    newPos : Point
                                    newPos =
                                        case containerPosOnTarget of
                                            Just cp ->
                                                Point (cp.x + 24) (cp.y + 24)

                                            Nothing ->
                                                defaultDropPos

                                    movedItem : MapItem
                                    movedItem =
                                        case existingOnTarget of
                                            -- Reuse the existing parent view: keep pos/assoc, just unhide & unpin
                                            Just existing ->
                                                { existing | hidden = False, pinned = False }

                                            -- Otherwise, create a fresh one based on the inner-map item
                                            Nothing ->
                                                let
                                                    adjustedProps =
                                                        case itemInContainer.props of
                                                            MapTopic tp ->
                                                                MapTopic { tp | pos = newPos }

                                                            other ->
                                                                other
                                                in
                                                { id = topicId
                                                , hidden = False
                                                , pinned = False
                                                , props = adjustedProps
                                                , parentAssocId = -1
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
