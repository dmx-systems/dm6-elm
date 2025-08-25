module CallGraph exposing
    ( CallGraph
    , Edge
    , calleesOf
    , callersOf
    , decode
    , get
    , neighbours
    , pathsFrom
    , pathsTo
    )

import Dict exposing (Dict)
import Http
import Json.Decode as Decode
import Set



-- DATA


type alias Edge =
    { from : String -- "Module.function"
    , to : String
    }


type alias CallGraph =
    { edges : List Edge
    }



-- LOADING


decode : Decode.Decoder CallGraph
decode =
    Decode.map CallGraph
        (Decode.field "edges" (Decode.list edgeDecoder))


edgeDecoder : Decode.Decoder Edge
edgeDecoder =
    Decode.map2 Edge
        (Decode.field "from" Decode.string)
        (Decode.field "to" Decode.string)


get : String -> (Result Http.Error CallGraph -> msg) -> Cmd msg
get url toMsg =
    Http.get { url = url, expect = Http.expectJson toMsg decode }



-- BASIC QUERIES


callersOf : CallGraph -> String -> List String
callersOf g name =
    g.edges
        |> List.filter (\e -> e.to == name)
        |> List.map .from
        |> unique


calleesOf : CallGraph -> String -> List String
calleesOf g name =
    g.edges
        |> List.filter (\e -> e.from == name)
        |> List.map .to
        |> unique


neighbours : CallGraph -> String -> { callers : List String, callees : List String }
neighbours g name =
    { callers = callersOf g name
    , callees = calleesOf g name
    }



-- PATH QUERIES


pathsFrom : CallGraph -> { maxDepth : Int } -> String -> List (List String)
pathsFrom g opts start =
    let
        adjacency =
            g.edges
                |> List.foldl
                    (\e d -> Dict.update e.from (appendUnique e.to) d)
                    Dict.empty
    in
    dfs adjacency opts.maxDepth start [] |> List.map List.reverse


pathsTo : CallGraph -> { maxDepth : Int } -> String -> List (List String)
pathsTo g opts target =
    let
        reverseAdj =
            g.edges
                |> List.foldl
                    (\e d -> Dict.update e.to (appendUnique e.from) d)
                    Dict.empty
    in
    dfs reverseAdj opts.maxDepth target [] |> List.map List.reverse



-- INTERNALS


dfs : Dict String (List String) -> Int -> String -> List String -> List (List String)
dfs adj depth current visited =
    let
        outs =
            Dict.get current adj |> Maybe.withDefault []

        nexts =
            outs |> List.filter (\n -> not (List.member n visited))
    in
    if depth <= 0 || List.isEmpty nexts then
        [ current :: visited ]

    else
        nexts
            |> List.concatMap (\n -> dfs adj (depth - 1) n (current :: visited))


appendUnique : String -> Maybe (List String) -> Maybe (List String)
appendUnique x maybe =
    case maybe of
        Nothing ->
            Just [ x ]

        Just xs ->
            if List.member x xs then
                Just xs

            else
                Just (x :: xs)


unique : List comparable -> List comparable
unique xs =
    Set.toList (Set.fromList xs)
