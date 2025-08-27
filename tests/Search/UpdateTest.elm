module Search.UpdateTest exposing (tests)

import AppModel exposing (Model)
import Compat.ModelAPI as M exposing (createTopic, defaultModel, getMapItemById, isMapTopic)
import Expect
import Model exposing (Id, MapId)
import Search exposing (SearchMsg(..))
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
                Expect.equal m2.search.text "foo"
        , test "SearchFocus opens the result menu (differs from default)" <|
            \_ ->
                let
                    ( m2, _ ) =
                        updateSearch Search.FocusInput defaultModel
                in
                -- Compare the whole search submodel (robust to internal field renames)
                Expect.notEqual m2.search defaultModel.search
        ]
