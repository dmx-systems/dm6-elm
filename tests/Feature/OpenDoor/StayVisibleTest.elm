module Feature.OpenDoor.StayVisibleTest exposing (tests)

import AppModel exposing (Model)
import Compat.ModelAPI as M exposing (createTopic, defaultModel, getMapItemById)
import Dict
import Expect
import Feature.OpenDoor.Move as OpenDoor
import Model exposing (..)
import Test exposing (..)


setup : ( Model, MapId, Id )
setup =
    let
        ( _, cId ) =
            createTopic "Container" Nothing defaultModel

        -- change the initial map placement:
        m2 =
            M.addItemToMapDefault cId 0 defaultModel

        m3 =
            { m2 | maps = Dict.insert cId (Map cId (Rectangle 0 0 0 0) Dict.empty) m2.maps }

        ( m4, tId ) =
            createTopic "A" Nothing m3

        m5 =
            M.addItemToMapDefault tId
                cId
                m4
    in
    ( m5, cId, tId )


tests : Test
tests =
    test "After OpenDoor.move: container still visible on parent, topic visible on parent" <|
        \_ ->
            let
                ( m0, containerId, topicId ) =
                    setup

                m1 =
                    OpenDoor.move { containerId = containerId, topicId = topicId, targetMapId = 0 } m0

                containerStillThere =
                    M.isItemInMap containerId 0 m1
                        && (getMapItemById containerId 0 m1.maps
                                |> Maybe.map .hidden
                                |> Maybe.withDefault True
                           )
                        == False

                topicVisibleOnParent =
                    M.isItemInMap topicId 0 m1
                        && (getMapItemById topicId 0 m1.maps
                                |> Maybe.map .hidden
                                |> Maybe.withDefault True
                           )
                        == False
            in
            Expect.equal ( containerStillThere, topicVisibleOnParent ) ( True, True )
