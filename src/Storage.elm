port module Storage exposing (modelDecoder, storeModel, storeModelWith)

import AppModel exposing (..)
import Compat.CoreModel as Core
import Compat.DmxImport as Dmx
import Compat.Storage as C
import Dict exposing (Dict)
import Json.Decode as D
import Json.Decode.Pipeline exposing (hardcoded, required)
import Json.Encode as E
import Model exposing (..)
import String



-- PORTS


port store : E.Value -> Cmd msg



-- SAVE HELPERS


storeModel : Model -> ( Model, Cmd Msg )
storeModel model =
    ( model, encodeModel model |> store )


storeModelWith : ( Model, Cmd Msg ) -> ( Model, Cmd Msg )
storeModelWith ( model, cmd ) =
    ( model
    , Cmd.batch
        [ cmd
        , encodeModel model |> store
        ]
    )



-- ENCODE MODEL -> JSON


encodeModel : Model -> E.Value
encodeModel model =
    E.object
        [ ( "items", model.items |> Dict.values |> E.list encodeItem )
        , ( "maps", model.maps |> Dict.values |> E.list encodeMap )
        , ( "mapPath", E.list E.int model.mapPath )
        , ( "nextId", E.int model.nextId )
        ]


encodeItem : Item -> E.Value
encodeItem item =
    case item.info of
        Topic topic ->
            E.object
                [ ( "topic"
                  , E.object
                        [ ( "id", E.int topic.id )
                        , ( "text", E.string topic.text )
                        , ( "icon", E.string <| Maybe.withDefault "" topic.iconName )
                        ]
                  )
                ]

        Assoc assoc ->
            E.object
                [ ( "assoc"
                  , E.object
                        [ ( "id", E.int assoc.id )
                        , ( "type", E.string assoc.itemType )
                        , ( "role1", E.string assoc.role1 )
                        , ( "player1", E.int assoc.player1 )
                        , ( "role2", E.string assoc.role2 )
                        , ( "player2", E.int assoc.player2 )
                        ]
                  )
                ]


encodeMap : Map -> E.Value
encodeMap map =
    E.object
        [ ( "id", E.int map.id )
        , ( "parentMapId", E.int map.parentMapId )
        , ( "rect"
          , E.object
                [ ( "x1", E.float map.rect.x1 )
                , ( "y1", E.float map.rect.y1 )
                , ( "x2", E.float map.rect.x2 )
                , ( "y2", E.float map.rect.y2 )
                ]
          )
        , ( "items", map.items |> Dict.values |> E.list encodeMapItem )
        ]


encodeMapItem : MapItem -> E.Value
encodeMapItem item =
    case item.props of
        MapTopic topicProps ->
            E.object
                [ ( "id", E.int item.id )
                , ( "parentAssocId", E.int item.parentAssocId )
                , ( "hidden", E.bool item.hidden )
                , ( "pinned", E.bool item.pinned )
                , ( "topicProps"
                  , E.object
                        [ ( "pos"
                          , E.object
                                [ ( "x", E.float topicProps.pos.x )
                                , ( "y", E.float topicProps.pos.y )
                                ]
                          )
                        , ( "size"
                          , E.object
                                [ ( "w", E.float topicProps.size.w )
                                , ( "h", E.float topicProps.size.h )
                                ]
                          )
                        , ( "display", encodeDisplayName topicProps.displayMode )
                        ]
                  )
                ]

        MapAssoc _ ->
            E.object
                [ ( "id", E.int item.id )
                , ( "parentAssocId", E.int item.parentAssocId )
                , ( "hidden", E.bool item.hidden )
                , ( "pinned", E.bool item.pinned )
                , ( "assocProps", E.object [] )
                ]


encodeDisplayName : DisplayMode -> E.Value
encodeDisplayName displayMode =
    E.string <|
        case displayMode of
            Monad LabelOnly ->
                "LabelOnly"

            Monad Detail ->
                "Detail"

            Container BlackBox ->
                "BlackBox"

            Container WhiteBox ->
                "WhiteBox"

            Container Unboxed ->
                "Unboxed"



-- DECODE JSON -> MODEL  (TOLERANT / BACKWARD-COMPAT)


modelDecoder : D.Decoder Model
modelDecoder =
    -- Try full decode; if flags are {}, fall back to AppModel.default
    D.oneOf
        [ dmxDecoder
        , fullModelDecoder
        , D.succeed default
        ]


fullModelDecoder : D.Decoder Model
fullModelDecoder =
    D.succeed Model
        |> required "items" itemsValueDecoder
        |> required "maps" mapsValueDecoder
        |> required "mapPath" (D.list D.int)
        |> required "nextId" D.int
        -- transient state/components: fresh each init
        |> hardcoded default.selection
        |> hardcoded default.editState
        |> hardcoded default.measureText
        |> hardcoded default.mouse
        |> hardcoded default.search
        |> hardcoded default.iconMenu



-- "items" can be a LIST (new) or a DICT (legacy). Normalize to Dict Id Item.


itemsValueDecoder : D.Decoder Items
itemsValueDecoder =
    D.oneOf
        [ D.list itemTupleDecoder |> D.andThen tupleListToItems
        , itemDictDecoder
        ]



-- For list-form "items": decode to (Id, ItemInfo)


itemTupleDecoder : D.Decoder ( Id, ItemInfo )
itemTupleDecoder =
    D.oneOf
        [ D.field "topic"
            (D.map2 Tuple.pair
                (D.field "id" D.int)
                (D.map Topic <|
                    D.map3 TopicInfo
                        (D.field "id" D.int)
                        (D.field "text" D.string)
                        (C.field2 "icon" "iconName" D.string |> D.andThen maybeString)
                )
            )
        , D.field "assoc"
            (D.map2 Tuple.pair
                (D.field "id" D.int)
                (D.map Assoc <|
                    D.map6 AssocInfo
                        (D.field "id" D.int)
                        (C.field2 "type" "itemType" D.string)
                        (D.field "role1" D.string)
                        (D.field "player1" D.int)
                        (D.field "role2" D.string)
                        (D.field "player2" D.int)
                )
            )
        ]



-- For dict-form "items": values carry either "topic" or "assoc"


itemDictDecoder : D.Decoder Items
itemDictDecoder =
    D.dict itemInfoDecoder
        |> D.andThen
            (\d ->
                d
                    |> Dict.values
                    |> List.map
                        (\info ->
                            case info of
                                Topic t ->
                                    ( t.id, info )

                                Assoc a ->
                                    ( a.id, info )
                        )
                    |> tupleListToItems
            )


itemInfoDecoder : D.Decoder ItemInfo
itemInfoDecoder =
    D.oneOf
        [ D.field "topic"
            (D.map Topic <|
                D.map3 TopicInfo
                    (D.field "id" D.int)
                    (D.field "text" D.string)
                    (C.field2 "icon" "iconName" D.string |> D.andThen maybeString)
            )
        , D.field "assoc"
            (D.map Assoc <|
                D.map6 AssocInfo
                    (D.field "id" D.int)
                    (C.field2 "type" "itemType" D.string)
                    (D.field "role1" D.string)
                    (D.field "player1" D.int)
                    (D.field "role2" D.string)
                    (D.field "player2" D.int)
            )
        ]



-- "maps" can be a LIST (new) or a DICT (legacy). Normalize to Dict Id Map.


mapsValueDecoder : D.Decoder (Dict Int Map)
mapsValueDecoder =
    D.oneOf
        [ D.list mapDecoder |> D.andThen recordsToDict
        , dictInt mapDecoder
        ]


mapDecoder : D.Decoder Map
mapDecoder =
    D.map4 Map
        (D.field "id" D.int)
        (D.field "parentMapId" D.int)
        (D.field "rect"
            (D.map4 Rectangle
                (D.field "x1" D.float)
                (D.field "y1" D.float)
                (D.field "x2" D.float)
                (D.field "y2" D.float)
            )
        )
        (D.field "items" mapItemsValueDecoder)



-- "map.items" can be LIST or DICT of MapItem


mapItemsValueDecoder : D.Decoder (Dict Int MapItem)
mapItemsValueDecoder =
    D.oneOf
        [ D.list mapItemDecoder |> D.andThen recordsToDict
        , dictInt mapItemDecoder
        ]


mapItemDecoder : D.Decoder MapItem
mapItemDecoder =
    D.map5 MapItem
        (D.field "id" D.int)
        (D.field "parentAssocId" D.int)
        (D.field "hidden" D.bool)
        (D.field "pinned" D.bool)
        (D.oneOf
            [ D.field "topicProps"
                (D.map MapTopic <|
                    D.map3 TopicProps
                        (D.field "pos"
                            (D.map2 Point
                                (D.field "x" D.float)
                                (D.field "y" D.float)
                            )
                        )
                        (D.field "size"
                            (D.map2 Size
                                (D.field "w" D.float)
                                (D.field "h" D.float)
                            )
                        )
                        (C.field2 "display" "displayMode" D.string
                            |> D.andThen displayModeFromString
                        )
                )
            , D.field "assocProps" (D.succeed (MapAssoc AssocProps))
            ]
        )



-- HELPERS (decoding)


tupleListToItems : List ( Id, ItemInfo ) -> D.Decoder Items
tupleListToItems tuples =
    tuples
        |> List.map (\( id, info ) -> ( id, Item id info ))
        |> Dict.fromList
        |> D.succeed


recordsToDict : List { r | id : Id } -> D.Decoder (Dict Int { r | id : Id })
recordsToDict recs =
    recs
        |> List.map (\r -> ( r.id, r ))
        |> Dict.fromList
        |> D.succeed



-- Convert a String-keyed JSON dict into a Dict Int …


dictInt : D.Decoder a -> D.Decoder (Dict Int a)
dictInt valueDecoder =
    D.dict valueDecoder
        |> D.andThen
            (\d ->
                let
                    toPair ( k, v ) =
                        case String.toInt k of
                            Just i ->
                                Just ( i, v )

                            Nothing ->
                                Nothing

                    pairs =
                        d |> Dict.toList |> List.filterMap toPair
                in
                if List.length pairs == Dict.size d then
                    D.succeed (Dict.fromList pairs)

                else
                    D.fail "Expected integer keys in object (got non-integer key)"
            )


displayModeFromString : String -> D.Decoder DisplayMode
displayModeFromString str =
    case str of
        "LabelOnly" ->
            D.succeed (Monad LabelOnly)

        "Detail" ->
            D.succeed (Monad Detail)

        "BlackBox" ->
            D.succeed (Container BlackBox)

        "WhiteBox" ->
            D.succeed (Container WhiteBox)

        "Unboxed" ->
            D.succeed (Container Unboxed)

        _ ->
            D.fail <| "\"" ++ str ++ "\" is an invalid display mode"


maybeString : String -> D.Decoder (Maybe String)
maybeString str =
    D.succeed <|
        if str == "" then
            Nothing

        else
            Just str


dmxDecoder : D.Decoder Model
dmxDecoder =
    D.value
        |> D.andThen
            (\v ->
                case Dmx.decodeCoreTopicToCore v of
                    Ok core ->
                        Core.toAppModel core |> D.succeed

                    Err _ ->
                        D.fail "not DMX JSON"
            )
