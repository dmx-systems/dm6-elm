module Feature.OpenDoor.CopyTest exposing (tests)

import AppModel exposing (Model, default)
import Dict
import Expect
import Feature.OpenDoor.Copy as Copy
import Model exposing (..)
import ModelAPI exposing (addItemToMap, createTopic, isItemInMap)
import Test exposing (..)



-- Build a model with a container and one child topic inside it.


setupModel : ( Model, MapId, Id )
setupModel =
    let
        ( m1, cId ) =
            createTopic "Container" Nothing default

        -- container visible on home map (0) as BlackBox
        m2 =
            addItemToMap cId
                (MapTopic (TopicProps (Point 100 100) (Size 160 60) (Container BlackBox)))
                0
                m1

        -- inner map for container (parent = 0)
        m3 =
            { m2 | maps = Dict.insert cId (Map cId (Rectangle 0 0 0 0) Dict.empty) m2.maps }

        -- child topic
        ( m4, tId ) =
            createTopic "Child" Nothing m3

        -- put child inside container
        m5 =
            addItemToMap tId
                (MapTopic (TopicProps (Point 30 30) (Size 120 40) (Monad LabelOnly)))
                cId
                m4
    in
    ( m5, cId, tId )


sizeOf : MapId -> Model -> Int
sizeOf mapId model =
    model.maps
        |> Dict.get mapId
        |> Maybe.map (\mp -> Dict.size mp.items)
        |> Maybe.withDefault -1


tests : Test
tests =
    describe "Feature.OpenDoor.Copy.copyFromContainer"
        [ test "copies topic from container into parent map but keeps it in the container" <|
            \_ ->
                let
                    ( m0, containerId, topicId ) =
                        setupModel

                    targetId =
                        0

                    preSizeSrc =
                        sizeOf containerId m0

                    preSizeTgt =
                        sizeOf targetId m0

                    -- perform the copy
                    m1 =
                        Copy.copyFromContainer
                            { containerId = containerId
                            , topicId = topicId
                            , targetMapId = targetId
                            }
                            m0

                    postSizeSrc =
                        sizeOf containerId m1

                    postSizeTgt =
                        sizeOf targetId m1
                in
                Expect.all
                    [ \_ -> Expect.equal (isItemInMap topicId containerId m1) True
                    , \_ -> Expect.equal (isItemInMap topicId targetId m1) True
                    , \_ -> Expect.equal postSizeSrc preSizeSrc
                    , \_ ->
                        let
                            expected =
                                if isItemInMap topicId targetId m0 then
                                    preSizeTgt

                                else
                                    preSizeTgt + 1
                        in
                        Expect.equal postSizeTgt expected
                    ]
                    ()

        -- ★ provide the subject
        ]
