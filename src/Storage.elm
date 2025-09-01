port module Storage exposing (modelDecoder, storeModel, storeModelWith)

import AppModel exposing (..)
import Dict exposing (Dict)
import Json.Decode as D
import Json.Encode as E
import Model exposing (..)
import String



-- PORTS


port store : E.Value -> Cmd msg



-- ENCODE/DECODE MODEL <-> JS VALUE (for storage)


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



-- Encode


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
    E.object
        [ case item.info of
            Topic topic ->
                ( "topic"
                , E.object
                    [ ( "id", E.int topic.id )
                    , ( "text", E.string topic.text )
                    , ( "icon", E.string <| Maybe.withDefault "" topic.iconName )
                    ]
                )

            Assoc assoc ->
                ( "assoc"
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
    E.object
        [ ( "id", E.int item.id )
        , ( "parentAssocId", E.int item.parentAssocId )
        , ( "hidden", E.bool item.hidden )
        , ( "pinned", E.bool item.pinned )
        , case item.props of
            MapTopic topicProps ->
                ( "topicProps"
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

            MapAssoc assosProps ->
                ( "assocProps"
                , E.object []
                )
        ]


encodeDisplayName : DisplayMode -> E.Value
encodeDisplayName displayMode =
    E.string
        (case displayMode of
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
        )



-- Decode
-- Public decoder: accepts "{}" via emptyObjectDefault, otherwise full schema


modelDecoder : D.Decoder Model
modelDecoder =
    D.oneOf
        [ emptyObjectDefault
        , modelDecoderFull
        ]


modelDecoderFull : D.Decoder Model
modelDecoderFull =
    D.map4 build
        itemsDecoder
        (D.oneOf
            [ D.field "maps" (D.list mapDecoder)
            , D.field "maps" (D.dict mapDecoder) |> D.map (Dict.values >> List.sortBy .id)
            , D.succeed []
            ]
        )
        (D.oneOf
            [ D.field "mapPath" (D.list D.int)
            , D.succeed [ 0 ]
            ]
        )
        (D.oneOf
            [ D.field "nextId" D.int
            , D.succeed 1
            ]
        )


build :
    Items
    -> List Map
    -> List MapId
    -> Id
    -> Model
build items mapsRaw mapPath nextId =
    let
        base =
            default

        mapsDict : Dict MapId Map
        mapsDict =
            mapsRaw
                |> List.map (\m -> ( m.id, m ))
                |> Dict.fromList

        m0 =
            { base
                | items = items
                , maps = mapsDict
                , mapPath = mapPath
                , nextId = nextId
            }
    in
    -- guarantee the home map exists even if {} was given
    ensureMapLocal 0 m0


itemDecoder : D.Decoder ( Id, ItemInfo )
itemDecoder =
    D.oneOf
        [ D.field "topic"
            (D.map2 Tuple.pair
                (D.field "id" D.int)
                (D.map Topic <|
                    D.map3 TopicInfo
                        (D.field "id" D.int)
                        (D.field "text" D.string)
                        (D.field "icon" D.string
                            |> D.andThen maybeString
                        )
                )
            )
        , D.field "assoc"
            (D.map2 Tuple.pair
                (D.field "id" D.int)
                (D.map Assoc <|
                    D.map6 AssocInfo
                        (D.field "id" D.int)
                        (D.field "type" D.string)
                        (D.field "role1" D.string)
                        (D.field "player1" D.int)
                        (D.field "role2" D.string)
                        (D.field "player2" D.int)
                )
            )
        ]


mapDecoder : D.Decoder Map
mapDecoder =
    D.map3 Map
        (D.field "id" D.int)
        (D.field "rect" <|
            D.map4 Rectangle
                (D.field "x1" D.float)
                (D.field "y1" D.float)
                (D.field "x2" D.float)
                (D.field "y2" D.float)
        )
        (D.field "items" (D.list mapItemDecoder |> D.andThen toDictDecoder))


mapItemDecoder : D.Decoder MapItem
mapItemDecoder =
    D.map5 MapItem
        (D.field "id" D.int)
        (D.field "parentAssocId" D.int)
        (D.field "hidden" D.bool)
        (D.field "pinned" D.bool)
        (D.oneOf
            [ D.field "topicProps" <|
                D.map MapTopic <|
                    D.map3 TopicProps
                        (D.field "pos" <|
                            D.map2 Point
                                (D.field "x" D.float)
                                (D.field "y" D.float)
                        )
                        (D.field "size" <|
                            D.map2 Size
                                (D.field "w" D.float)
                                (D.field "h" D.float)
                        )
                        (D.field "display" D.string |> D.andThen displayModeDecoder)
            , D.field "assocProps" <| D.succeed (MapAssoc AssocProps)
            ]
        )


tupleToDictDecoder : List ( Id, ItemInfo ) -> D.Decoder Items
tupleToDictDecoder tuples =
    tuples
        |> List.map (\( id, info ) -> ( id, Item id info ))
        |> Dict.fromList
        |> D.succeed


toDictDecoder : List { item | id : Id } -> D.Decoder (Dict Int { item | id : Id })
toDictDecoder items =
    items
        |> List.map (\item -> ( item.id, item ))
        |> Dict.fromList
        |> D.succeed


displayModeDecoder : String -> D.Decoder DisplayMode
displayModeDecoder str =
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
    D.succeed
        (case str of
            "" ->
                Nothing

            _ ->
                Just str
        )



-- If the JSON root is `{}`, yield Defaults.default with a guaranteed home map (id 0)


emptyObjectDefault : D.Decoder Model
emptyObjectDefault =
    D.dict D.value
        |> D.andThen
            (\root ->
                if Dict.isEmpty root then
                    D.succeed (ensureMapLocal 0 default)

                else
                    D.fail "not empty"
            )



-- Local: guarantee that a map exists (used to ensure home map on {} decode)


ensureMapLocal : MapId -> Model -> Model
ensureMapLocal mapId m =
    if Dict.member mapId m.maps then
        m

    else
        { m
            | maps =
                Dict.insert
                    mapId
                    (Model.Map mapId (Model.Rectangle 0 0 0 0) Dict.empty)
                    m.maps
        }



-- turn Dict String v into Dict Int v (drops non-int keys)


mapKeysStringToInt : Dict.Dict String v -> Dict.Dict Int v
mapKeysStringToInt =
    Dict.foldl
        (\k v acc ->
            case String.toInt k of
                Just i ->
                    Dict.insert i v acc

                Nothing ->
                    acc
        )
        Dict.empty


itemsDecoder : D.Decoder Items
itemsDecoder =
    D.oneOf
        [ D.field "items" (D.dict itemInfoDecoder)
            |> D.map mapKeysStringToIntItems
        , D.field "items" (D.list itemTupleDecoder)
            |> D.andThen tupleToDictDecoder
        , D.succeed Dict.empty
        ]


itemTupleDecoder : D.Decoder ( Id, ItemInfo )
itemTupleDecoder =
    itemDecoder


itemInfoDecoder : D.Decoder ItemInfo
itemInfoDecoder =
    D.oneOf
        [ D.field "topic"
            (D.map Topic <|
                D.map3 TopicInfo
                    (D.field "id" D.int)
                    (D.field "text" D.string)
                    (D.field "icon" D.string |> D.andThen maybeString)
            )
        , D.field "assoc"
            (D.map Assoc <|
                D.map6 AssocInfo
                    (D.field "id" D.int)
                    (D.field "type" D.string)
                    (D.field "role1" D.string)
                    (D.field "player1" D.int)
                    (D.field "role2" D.string)
                    (D.field "player2" D.int)
            )
        ]



-- turn Dict String ItemInfo into Items (Dict Int Item)


mapKeysStringToIntItems : Dict.Dict String ItemInfo -> Items
mapKeysStringToIntItems =
    Dict.foldl
        (\k info acc ->
            case String.toInt k of
                Just i ->
                    Dict.insert i (Item i info) acc

                Nothing ->
                    acc
        )
        Dict.empty
