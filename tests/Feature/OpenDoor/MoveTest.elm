module Feature.OpenDoor.MoveTest exposing (tests)

import AppModel exposing (Model)
import Compat.ModelAPI as M exposing (addItemToMapDefault, createTopic, defaultModel, getMapItemById, isMapTopic)
import Model
    exposing
        ( Id
        , Item(..)
          -- gives you the Topic constructor
        , Map
        , MapId
        , MapItem
        , Point
        , Rectangle
        , TopicProps
        )
import Test exposing (..)



-- Build a model with a container and one child topic inside it.


setupModel : ( Model, MapId, Id )
setupModel =
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
    describe "Feature.OpenDoor.move"
        [ test "moves topic from container to parent map (visible outside, absent inside)" <|
            \_ ->
                let
                    ( m0, containerId, topicId ) =
                        setupModel

                    -- perform the move to parent map = 0 in this setup
                    m1 =
                        OpenDoor.move
                            { containerId = containerId
                            , topicId = topicId
                            , targetMapId = 0
                            }
                            m0

                    -- visible on the parent (home = 0), and not hidden
                    outsideVisible =
                        isItemInMap topicId 0 m1
                            && (getMapItemById topicId 0 m1.maps
                                    |> Maybe.map .hidden
                                    |> Maybe.withDefault True
                               )
                            == False

                    -- completely absent from the container’s inner map
                    insideAbsent =
                        getMapItemById topicId containerId m1.maps == Nothing
                in
                Expect.equal ( outsideVisible, insideAbsent ) ( True, True )
        ]
