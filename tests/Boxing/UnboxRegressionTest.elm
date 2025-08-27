module Boxing.UnboxRegressionTest exposing (tests)

import AppModel exposing (Model)
import Boxing exposing (unboxContainer)
import Compat.ModelAPI as M exposing (createTopic, defaultModel, getMapItemById, isMapTopic)
import Dict
import Expect
import Model exposing (Id, Map, MapId, MapItem, MapProps(..), Rectangle)
import Test exposing (..)


setup : ( Model, MapId, Id )
setup =
    let
        ( _, cId ) =
            createTopic "Container" Nothing defaultModel

        m2 =
            M.addItemToMapDefault cId 0 defaultModel

        m3 =
            { m2 | maps = Dict.insert cId (Map cId Dict.empty (Rectangle 0 0 0 0) 0) m2.maps }

        ( m4, tId ) =
            createTopic "Child" Nothing m3

        m5 =
            M.addItemToMapDefault tId
                cId
                m4
    in
    ( m5, cId, tId )


tests : Test
tests =
    describe "unboxContainer regression (ace1eba)"
        [ test "reveals a Topic (not an Assoc) on the parent map and shows its composition assoc" <|
            \_ ->
                let
                    ( m0, containerId, topicId ) =
                        setup

                    maps1 =
                        unboxContainer containerId 0 m0

                    m1 =
                        { m0 | maps = maps1 }

                    -- The revealed item on the parent map must be a Topic and visible.
                    topicOnParentIsVisibleTopic =
                        case getMapItemById topicId 0 m1.maps of
                            Just mi ->
                                isMapTopic mi && not mi.hidden

                            Nothing ->
                                False

                    -- Its composition association should also be visible on the parent map.
                    assocOnParentVisible =
                        case getMapItemById topicId containerId m0.maps of
                            Just childInContainer ->
                                let
                                    assocId =
                                        childInContainer.parentAssocId
                                in
                                getMapItemById assocId 0 m1.maps
                                    |> Maybe.map (\ma -> isMapAssoc ma && not ma.hidden)
                                    |> Maybe.withDefault False

                            Nothing ->
                                False
                in
                Expect.equal ( topicOnParentIsVisibleTopic, assocOnParentVisible )
                    ( True, True )
        ]



-- local helper replacing the missing isMapAssoc


isMapAssoc : MapItem -> Bool
isMapAssoc mi =
    case mi.props of
        MapAssoc _ ->
            True

        _ ->
            False
