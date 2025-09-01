module Model.SelfContainmentInvariantTest exposing (tests)

import AppModel exposing (Model, default)
import Dict
import Expect
import Model exposing (..)
import ModelAPI exposing (addItemToMap, createMap, createTopic)
import Test exposing (..)



-- Global invariant helper: a map must NOT contain itself.


selfContainedMaps : Model -> List MapId
selfContainedMaps m =
    m.maps
        |> Dict.values
        |> List.filter (\mp -> Dict.member mp.id mp.items)
        |> List.map .id


tests : Test
tests =
    describe "Global invariant: no map contains itself"
        [ test "deliberate self-containment is detected" <|
            \_ ->
                let
                    -- 1) create a container topic and show it on the home map (0)
                    ( m1, cId ) =
                        createTopic "Container" Nothing default

                    m2 =
                        addItemToMap cId
                            (MapTopic (TopicProps (Point 100 100) (Size 60 160) (Container BlackBox)))
                            0
                            m1

                    -- 2) create its inner map
                    m3 =
                        createMap cId m2

                    -- 3) (INTENTIONALLY BAD) add the container topic to its own inner map
                    badModel =
                        addItemToMap cId
                            (MapTopic (TopicProps (Point 0 0) (Size 10 10) (Monad LabelOnly)))
                            cId
                            m3
                in
                -- Expect the violation: map cId contains itself
                Expect.equal [ cId ] (selfContainedMaps badModel)
        ]
