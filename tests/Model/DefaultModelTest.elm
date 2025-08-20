module Model.DefaultModelTest exposing (tests)

import Compat.ModelAPI exposing (defaultModel)
import Dict
import Expect
import Model
import Test exposing (..)


tests : Test
tests =
    describe "defaultModel"
        [ test "has root mapPath [0]" <|
            \_ -> Expect.equal defaultModel.mapPath [ 0 ]
        , test "has root map (id 0)" <|
            \_ ->
                case Dict.get 0 defaultModel.maps of
                    Just root ->
                        -- parentMapId was removed; assert root id instead
                        Expect.equal root.id 0

                    Nothing ->
                        Expect.fail "Root map 0 not found"
        , test "starts empty selection and items" <|
            \_ ->
                Expect.all
                    [ \_ -> Expect.equal defaultModel.selection []
                    , \_ -> Expect.equal (Dict.size defaultModel.items) 0
                    ]
                    ()
        , test "search text and measure text are empty" <|
            \_ ->
                Expect.all
                    [ \_ -> Expect.equal defaultModel.search.text ""
                    , \_ -> Expect.equal defaultModel.measureText ""
                    ]
                    ()
        , test "nextId starts at 1" <|
            \_ -> Expect.equal defaultModel.nextId 1
        ]
