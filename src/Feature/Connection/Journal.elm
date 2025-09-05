module Feature.Connection.Journal exposing (Entry(..), Path, record, viewList, viewSketch)

import Html exposing (..)
import Html.Attributes exposing (..)
import Model exposing (Id, MapId, Point)
import ModelAPI exposing (MapPath)
import Svg exposing (..)
import Svg.Attributes exposing (..)


type alias Path =
    { selection : Maybe ( Id, MapPath ) -- what UI focused on (anchor)
    , computed : List MapPath -- what findPaths returned
    }


type Entry
    = CrossIn { topicId : Id, from : MapId, to : MapId, pos : Point, path : Path, note : String }
    | CrossOut { topicId : Id, from : MapId, to : MapId, pos : Point, path : Path, note : String }
    | CrossNoop { reason : String, topicId : Id, parent : MapId }
    | ErrorText String


record : Entry -> List Entry -> List Entry
record e acc =
    e :: acc |> List.take 200



-- keep it bounded


viewList : List Entry -> Html msg
viewList entries =
    div
        [ style "font-family" "monospace"
        , style "font-size" "12px"
        , style "max-height" "220px"
        , style "overflow" "auto"
        , style "border" "1px solid #ddd"
        , style "padding" "6px"
        ]
        (entries |> List.map viewLine)


viewLine : Entry -> Html msg
viewLine e =
    case e of
        CrossIn r ->
            div []
                [ text <|
                    "IN: topic "
                        ++ String.fromInt r.topicId
                        ++ " "
                        ++ fromTo r.from r.to
                        ++ " pos="
                        ++ showPt r.pos
                        ++ note r.note
                ]

        CrossOut r ->
            div []
                [ text <|
                    "OUT: topic "
                        ++ String.fromInt r.topicId
                        ++ " "
                        ++ fromTo r.from r.to
                        ++ " pos="
                        ++ showPt r.pos
                        ++ note r.note
                ]

        CrossNoop r ->
            div []
                [ text <|
                    "NO-OP: topic "
                        ++ String.fromInt r.topicId
                        ++ " parent="
                        ++ String.fromInt r.parent
                        ++ " reason="
                        ++ r.reason
                ]

        ErrorText s ->
            div [ style "color" "#a00" ] [ text ("ERR: " ++ s) ]


fromTo : MapId -> MapId -> String
fromTo a b =
    "(from " ++ String.fromInt a ++ " → " ++ String.fromInt b ++ ")"


showPt : Point -> String
showPt p =
    "(" ++ String.fromFloat p.x ++ "," ++ String.fromFloat p.y ++ ")"


note : String -> String
note s =
    if s == "" then
        ""

    else
        "  · " ++ s



-- very small, schematic path sketch: each MapPath becomes a dot row


viewSketch : List Entry -> Html msg
viewSketch entries =
    let
        dots =
            entries
                |> List.take 1
                -- sketch most recent only (cheap & legible)
                |> List.concatMap
                    (\e ->
                        case e of
                            CrossIn r ->
                                r.path.computed

                            CrossOut r ->
                                r.path.computed

                            _ ->
                                []
                    )
    in
    svg [ width "160", height "60", viewBox "0 0 160 60", style "border" "1px solid #eee" ]
        (dots
            |> List.indexedMap
                (\row path ->
                    let
                        y =
                            20 + toFloat row * 16

                        xs =
                            List.indexedMap (\i _ -> 10 + toFloat i * 18) path
                    in
                    List.concat
                        [ [ Svg.text_ [ x "4", y (String.fromFloat (y - 7)), fontSize "8" ] [ Svg.text "path" ] ]
                        , xs
                            |> List.map
                                (\xv -> circle [ cx (String.fromFloat xv), cy (String.fromFloat y), r "3", fill "#333" ] [])
                        ]
                )
            |> List.concat
        )
