module Model.AddItemToMapCycleTest exposing (tests)

import AppModel exposing (Model, default)
import Compat.ModelAPI as ModelAPI exposing (addItemToMap, defaultProps)
import Dict
import Expect
import Model exposing (MapProps(..), Size)
import Test exposing (..)


{-| Build a model that has an empty child map with id=1 (no parent field needed).
-}
seedModelWithMaps : Model
seedModelWithMaps =
    let
        base =
            default

        root0 =
            case Dict.get 0 base.maps of
                Just m ->
                    m

                Nothing ->
                    Debug.todo "root map id=0 missing"

        -- child map id=1 (empty)
        map1 =
            { root0 | id = 1, items = Dict.empty }
    in
    { base | maps = base.maps |> Dict.insert 1 map1 }


tests : Test
tests =
    describe "addItemToMap cycle-guard"
        [ test "refuses direct self-containment (A→A)" <|
            \_ ->
                let
                    model0 =
                        seedModelWithMaps

                    tp =
                        defaultProps 1 (Size 0 0) model0

                    props : MapProps
                    props =
                        MapTopic tp

                    -- try to place map 1 into map 1
                    model1 =
                        ModelAPI.addItemToMap 1 props 1 model0
                in
                -- must be rejected -> model unchanged
                Expect.equal model1 model0
        , test "refuses ancestral cycle (A→descendant(A))" <|
            \_ ->
                let
                    base =
                        default

                    root0 =
                        case Dict.get 0 base.maps of
                            Just m ->
                                m

                            Nothing ->
                                Debug.todo "root map id=0 missing"

                    -- two empty maps
                    map1 =
                        { root0 | id = 1, items = Dict.empty }

                    map2 =
                        { root0 | id = 2, items = Dict.empty }

                    model0 =
                        { base
                            | maps =
                                base.maps
                                    |> Dict.insert 1 map1
                                    |> Dict.insert 2 map2
                        }

                    -- establish 1 → 2 (put 2 inside 1)
                    tp2 =
                        defaultProps 2 (Size 0 0) model0

                    props2 : MapProps
                    props2 =
                        MapTopic tp2

                    model1 =
                        addItemToMap 2 props2 1 model0

                    -- now try to create a cycle by putting 1 inside 2 (must be rejected)
                    tp1 =
                        defaultProps 1 (Size 0 0) model1

                    props1 : MapProps
                    props1 =
                        MapTopic tp1

                    model2 =
                        addItemToMap 1 props1 2 model1

                    itemsIn2After =
                        case Dict.get 2 model2.maps of
                            Just m ->
                                m.items

                            Nothing ->
                                Dict.empty
                in
                -- item 1 must NOT appear in map 2
                Expect.equal (Dict.member 1 itemsIn2After) False
        ]
