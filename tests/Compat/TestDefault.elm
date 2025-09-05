module Compat.TestDefault exposing (defaultModel, suite)

import AppMain as AdapterMain
import Dict
import Expect
import Json.Encode as E
import Main as CoreMain
import Test exposing (..)



-- Provide a canonical empty model for tests that need a ready-to-use default.
-- Uses CoreMain.init with explicit Flags (stored="{}").


defaultModel : CoreMain.Model
defaultModel =
    Tuple.first (CoreMain.init { slug = "test", stored = "{}" })



-- Sanity checks for the adapter that accepts Json.Value (E.null → cold boot)


suite : Test
suite =
    describe "Compat default boot via adapter"
        [ test "init with E.null cold-boots to default model" <|
            \_ ->
                let
                    m =
                        Tuple.first (AdapterMain.init E.null)
                in
                Expect.equal [ 0 ] m.mapPath
        , test "home map (0) exists" <|
            \_ ->
                let
                    m =
                        Tuple.first (AdapterMain.init E.null)
                in
                Expect.equal True (Dict.member 0 m.maps)
        , test "nextId starts at 1" <|
            \_ ->
                let
                    m =
                        Tuple.first (AdapterMain.init E.null)
                in
                Expect.equal 1 m.nextId
        ]
