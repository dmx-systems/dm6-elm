module Feature.OpenDoor.MoveTopicOutOfContainerTest exposing (tests)

import AppModel as AM
import Expect
import Json.Decode as D
import Main exposing (moveTopicToMap)
import Model exposing (Point)
import ModelAPI
    exposing
        ( createMap
        , createTopicIn
        , getMap
        , getMapItem
        )
import Storage exposing (modelDecoder)
import Test exposing (..)


emptyRootModel : AM.Model
emptyRootModel =
    let
        json =
            """
            {
              "nextId": 1,
              "items": {},
              "maps": {
                "0": { "id": 0, "items": {}, "hidden": [] }
              },
              "selection": { "mapId": 0, "topicId": null, "assocId": null }
            }
            """
    in
    case D.decodeString modelDecoder json of
        Ok m ->
            m

        Err err ->
            Debug.todo ("Bad emptyRootModel fixture: " ++ D.errorToString err)


testModelWithNestedTopic : AM.Model
testModelWithNestedTopic =
    emptyRootModel
        |> createTopicIn "A" Nothing [ 0 ]
        -- A gets id 1
        |> createMap 1
        -- ensure A's inner map exists
        |> createTopicIn "B" Nothing [ 1 ]



-- B gets id 2, placed inside A (map 1)


tests : Test
tests =
    describe "moveTopicToMap inner→parent"
        [ test "dragging B out of A places it in root map (0)" <|
            \_ ->
                let
                    initial =
                        testModelWithNestedTopic

                    newModel =
                        moveTopicToMap
                            2
                            1
                            (Point 0 0)
                            0
                            [ 0 ]
                            (Point 100 100)
                            initial

                    result =
                        ModelAPI.getMap 0 newModel.maps
                            |> Maybe.andThen (getMapItem 2)
                in
                case result of
                    Just _ ->
                        Expect.pass

                    Nothing ->
                        Expect.fail "Topic B (id 2) was not found in root map after move"
        , test "dragging B out of A with buggy targetId=topicId form leaves it missing (blocked)" <|
            \_ ->
                let
                    initial =
                        testModelWithNestedTopic

                    newModel =
                        moveTopicToMap
                            2
                            -- topicId B
                            1
                            -- fromMapId
                            (Point 0 0)
                            2
                            -- BUG: targetId = topicId
                            [ 0 ]
                            -- targetMapId = root
                            (Point 100 100)
                            initial

                    result =
                        getMap 0 newModel.maps
                            |> Maybe.andThen (getMapItem 2)
                in
                case result of
                    Just _ ->
                        Expect.fail "Buggy message still placed topic B in root, expected it to be missing"

                    Nothing ->
                        Expect.pass
        ]
