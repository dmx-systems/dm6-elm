module Feature.OpenDoor.Move exposing (move)

import AppModel exposing (Model)
import Config exposing (topicSize, whiteBoxPadding)
import Dict exposing (Dict)
import Model
    exposing
        ( Id
        , Map
        , MapId
        , MapItem
        , MapProps(..)
          -- ⬅️ gives MapTopic constructor
        , Point
        , Rectangle
        , TopicProps
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

                    Just itemInContainer ->
                        -- ⬅️ FIX: only one arg to Just
                        let
                            newItemsInContainer : Dict Id MapItem
                            newItemsInContainer =
                                Dict.remove topicId containerMap.items

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
                        case Dict.get targetMapId model1.maps of
                            Nothing ->
                                model1

                            Just targetMap ->
                                let
                                    existingOnTarget : Maybe MapItem
                                    existingOnTarget =
                                        Dict.get topicId targetMap.items

                                    containerPosOnTarget : Maybe Point
                                    containerPosOnTarget =
                                        case ModelAPI.getTopicProps containerId targetMapId model1.maps of
                                            Just tp ->
                                                Just tp.pos

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
                                in
                                case existingOnTarget of
                                    Just existing ->
                                        { model1
                                            | maps =
                                                Dict.insert
                                                    targetMapId
                                                    { targetMap
                                                        | items =
                                                            Dict.insert
                                                                topicId
                                                                { existing | hidden = False, pinned = False }
                                                                targetMap.items
                                                    }
                                                    model1.maps
                                        }

                                    Nothing ->
                                        -- Build props via API and insert as MapTopic
                                        let
                                            tp0 : TopicProps
                                            tp0 =
                                                ModelAPI.defaultProps topicId topicSize model1

                                            tp1 : TopicProps
                                            tp1 =
                                                { tp0 | pos = newPos }
                                        in
                                        ModelAPI.addItemToMap topicId (MapTopic tp1) targetMapId model1
