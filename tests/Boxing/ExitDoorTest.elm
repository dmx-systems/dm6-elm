module Boxing.ExitDoorTest exposing (tests)

import Boxing exposing (exitContainer)
import Dict
import Expect
import Model exposing (..)
import Test exposing (..)


setup : ( Model, MapId, Id )
setup =
    let
        -- start with default model
        ( m1, cId ) =
            createTopic "Container" Nothing defaultModel

        -- container visible on home map (0) as BlackBox
        m2 =
            addItemToMap cId
                (MapTopic (TopicProps (Point 100 100) (Size 160 60) (Container BlackBox)))
                0
                m1

        -- give the container its inner map (parent = 0)
        m3 =
            { m2 | maps = Dict.insert cId (Map cId Dict.empty (Rectangle 0 0 0 0) 0) m2.maps }

        -- create a child topic inside the container
        ( m4, tId ) =
            createTopic "Child" Nothing m3

        m5 =
            addItemToMap tId
                (MapTopic (TopicProps (Point 30 30) (Size 120 40) (Monad LabelOnly)))
                cId
                m4
    in
    ( m5, cId, tId )


tests : Test
tests =
    describe "exitContainer"
        [ test "moves selected topic from container to parent map (visible outside, hidden inside)" <|
            \_ ->
                let
                    ( m0, containerId, topicId ) =
                        setup

                    m1 =
                        exitContainer containerId topicId m0

                    outsideVisible =
                        isItemInMap topicId 0 m1
                            && (getMapItemById topicId 0 m1.maps
                                    |> Maybe.map .hidden
                                    |> Maybe.withDefault True
                               )
                            == False

                    insideHidden =
                        getMapItemById topicId containerId m1.maps
                            |> Maybe.map .hidden
                            |> Maybe.withDefault False
                in
                Expect.equal ( outsideVisible, insideHidden ) ( True, True )
        ]
