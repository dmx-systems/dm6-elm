module Search.UpdateTest exposing (tests)

import AppModel exposing (Model)
import Compat.ModelAPI as M exposing (createTopic, defaultModel, getMapItemById, isMapTopic)
import Expect
import Model exposing (Id, MapId)
import Search exposing (SearchMsg)
import SearchAPI exposing (updateSearch)
import Test exposing (..)


tests : Test
tests =
    describe "Search.updateSearch"
        [ test "SearchInput updates searchText" <|
            \_ ->
                let
                    ( m2, _ ) =
                        updateSearch (Input "foo") defaultModel
                in
                Expect.equal m2.searchText "foo"
        , test "SearchFocus opens the result menu (differs from default)" <|
            \_ ->
                let
                    ( m2, _ ) =
                        updateSearch SearchFocus defaultModel
                in
                Expect.notEqual m2.searchMenu defaultModel.searchMenu
        ]
