module Domain.ReparentRulesTest exposing (tests)

import Domain.Reparent as R
import Expect
import Test exposing (..)



-- Test-only parent relation: (child, parent)


type alias Parents =
    List ( String, String )


parentsOf : Parents -> String -> List String
parentsOf rel child =
    rel
        |> List.filter (\( c, _ ) -> c == child)
        |> List.map Tuple.second



-- Local wrapper so we can pass the relation as "model"


canReparent : String -> Maybe String -> Parents -> Result String ()
canReparent a b rel =
    R.canReparent a b (parentsOf rel)


tests : Test
tests =
    describe "Reparenting rules (pure)"
        [ test "A -> A is invalid" <|
            \_ ->
                canReparent "A" (Just "A") []
                    |> Expect.err
        , test "A -> descendant(A) is invalid" <|
            \_ ->
                -- B is descendant of A: A <- B
                canReparent "A" (Just "B") [ ( "B", "A" ) ]
                    |> Expect.err
        , test "A -> ancestor(A) is valid" <|
            \_ ->
                -- A has parent B: A <- B
                canReparent "A" (Just "B") [ ( "A", "B" ) ]
                    |> Expect.ok
        , test "A -> unrelated C is valid" <|
            \_ ->
                canReparent "A" (Just "C") []
                    |> Expect.ok
        , test "A -> Nothing (root) is valid" <|
            \_ ->
                canReparent "A" Nothing []
                    |> Expect.ok
        , test "OpenDoor: A inside B -> move A out to root is valid" <|
            \_ ->
                canReparent "A" Nothing [ ( "A", "B" ) ]
                    |> Expect.ok
        , test "A inside B -> attempt B under A is invalid (cycle)" <|
            \_ ->
                canReparent "B" (Just "A") [ ( "A", "B" ) ]
                    |> Expect.err
        ]
