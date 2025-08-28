module Boxing.UnboxRegressionTest exposing (tests)

import AppModel exposing (Model)
import Boxing exposing (unboxContainer)
import Compat.ModelAPI
    exposing
        ( addItemToMapDefault
        , createTopic
        , defaultModel
        , getMapItemById
        , isMapTopic
        )
import Expect
import Model exposing (Id)
import ModelAPI exposing (isMapAssoc, updateMapRect)
import Test exposing (..)


type alias Setup =
    { model : Model
    , parentMapId : Id
    , containerId : Id
    , topicId : Id
    }


setup : Setup
setup =
    let
        m0 =
            defaultModel

        ( m1, containerId ) =
            createTopic "Container" Nothing m0

        m2 =
            addItemToMapDefault containerId 0 m1

        ( m3, topicId ) =
            createTopic "Child" Nothing m2

        m4 =
            addItemToMapDefault topicId containerId m3

        m5 =
            updateMapRect containerId (\_ -> { x1 = 0, y1 = 0, x2 = 300, y2 = 200 }) m4
    in
    { model = m5
    , parentMapId = 0
    , containerId = containerId
    , topicId = topicId
    }


tests : Test
tests =
    describe "unboxContainer regression (ace1eba)"
        [ test "reveals a Topic (not an Assoc) on the parent map and shows its composition assoc" <|
            \_ ->
                let
                    s =
                        setup

                    mapsAfterUnbox =
                        unboxContainer s.containerId s.parentMapId s.model

                    -- FIX: record update must use a simple variable on the left
                    baseModel =
                        s.model

                    mAfter =
                        { baseModel | maps = mapsAfterUnbox }

                    topicOnParentIsVisibleTopic =
                        case getMapItemById s.topicId s.parentMapId mAfter.maps of
                            Just mi ->
                                isMapTopic mi && not mi.hidden

                            Nothing ->
                                False

                    assocOnParentVisible =
                        case getMapItemById s.topicId s.containerId s.model.maps of
                            Just childInContainer ->
                                let
                                    assocId =
                                        childInContainer.parentAssocId
                                in
                                getMapItemById assocId s.parentMapId mAfter.maps
                                    |> Maybe.map (\ma -> isMapAssoc ma && not ma.hidden)
                                    |> Maybe.withDefault False

                            Nothing ->
                                False
                in
                Expect.equal ( topicOnParentIsVisibleTopic, assocOnParentVisible )
                    ( True, True )
        ]
