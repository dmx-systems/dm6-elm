module Model.AddItemToMapCycleTest exposing (tests)

import AppModel exposing (Model, default)
import Dict
import Expect
import Model exposing (MapProps(..), Size)
import ModelAPI exposing (addItemToMap, defaultProps)
import Test exposing (..)


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

        -- child map id=1 under root
        map1 =
            { root0 | id = 1, parentMapId = 0, items = Dict.empty }
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

                    model1 =
                        addItemToMap 1 props 1 model0
                in
                Expect.equal model1 model0
        , test "refuses ancestral cycle (A→descendant(A))" <|
            \_ ->
                let
                    base =
                        default

                    -- take the existing root map (id = 0) as a template
                    root0 =
                        case Dict.get 0 base.maps of
                            Just m ->
                                m

                            Nothing ->
                                Debug.todo "root map id=0 missing"

                    -- build child (id=1) under root and grandchild (id=2) under child
                    map1 =
                        { root0 | id = 1, parentMapId = 0, items = Dict.empty }

                    map2 =
                        { root0 | id = 2, parentMapId = 1, items = Dict.empty }

                    model0 =
                        { base
                            | maps =
                                base.maps
                                    |> Dict.insert 1 map1
                                    |> Dict.insert 2 map2
                        }

                    -- try to add root (0) into descendant map (2) -> should be rejected
                    tp =
                        defaultProps 0 (Size 0 0) model0

                    props : MapProps
                    props =
                        MapTopic tp

                    model1 =
                        addItemToMap 0 props 2 model0

                    itemsIn2After =
                        case Dict.get 2 model1.maps of
                            Just m ->
                                m.items

                            Nothing ->
                                Dict.empty
                in
                -- Focused invariant: item 0 must NOT appear in map 2
                Expect.equal (Dict.member 0 itemsIn2After) False
        ]
