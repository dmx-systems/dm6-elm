port module Storage exposing (modelDecoder, storeModel, storeModelWith)

import AppModel exposing (..)
import Dict exposing (Dict)
import Json.Decode as D
import Json.Decode.Pipeline exposing (hardcoded, required)
import Json.Encode as E
import Model exposing (..)



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
        [ ( "items"
          , model.items
                |> E.dict
                    String.fromInt
                    encodeItem
          )
        , ( "maps"
          , model.maps
                |> E.dict
                    String.fromInt
                    encodeMap
          )
        , ( "mapPath", E.list E.int model.mapPath )
        , ( "nextId", E.int model.nextId )
        ]


encodeItem : Item -> E.Value
encodeItem item =
    E.object
        [ case item of
            Topic topic ->
                ( "topic"
                , E.object
                    [ ( "id", E.int topic.id )
                    , ( "text", E.string topic.text )
                    , ( "iconName", E.string <| Maybe.withDefault "" topic.iconName )
                    ]
                )

            Assoc assoc ->
                ( "assoc"
                , E.object
                    [ ( "id", E.int assoc.id )
                    , ( "itemType", E.string assoc.itemType )
                    , ( "player1", E.int assoc.player1 )
                    , ( "role1", E.string assoc.role1 )
                    , ( "player2", E.int assoc.player2 )
                    , ( "role2", E.string assoc.role2 )
                    ]
                )
        ]


encodeMap : Map -> E.Value
encodeMap map =
    E.object
        [ ( "id", E.int map.id )
        , ( "items"
          , map.items
                |> E.dict
                    String.fromInt
                    encodeMapItem
          )
        , ( "rect"
          , E.object
                [ ( "x1", E.float map.rect.x1 )
                , ( "y1", E.float map.rect.y1 )
                , ( "x2", E.float map.rect.x2 )
                , ( "y2", E.float map.rect.y2 )
                ]
          )
        , ( "parentMapId", E.int map.parentMapId )
        ]


encodeMapItem : MapItem -> E.Value
encodeMapItem item =
    E.object
        [ ( "id", E.int item.id )
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
                    , ( "displayMode", encodeDisplayName topicProps.displayMode )
                    ]
                )

            MapAssoc assosProps ->
                ( "assocProps"
                , E.object []
                )
        , ( "parentAssocId", E.int item.parentAssocId )
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


modelDecoder : D.Decoder Model
modelDecoder =
    D.succeed Model
        |> required "items"
            (D.dict
                (D.oneOf
                    [ D.field "topic" <|
                        D.map Topic <|
                            D.map3 TopicInfo
                                (D.field "id" D.int)
                                (D.field "text" D.string)
                                (D.field "iconName" D.string
                                    |> D.andThen maybeString
                                )
                    , D.field "assoc" <|
                        D.map Assoc <|
                            D.map6 AssocInfo
                                (D.field "id" D.int)
                                (D.field "itemType" D.string)
                                (D.field "player1" D.int)
                                (D.field "role1" D.string)
                                (D.field "player2" D.int)
                                (D.field "role2" D.string)
                    ]
                )
                |> D.andThen strToIntDictDecoder
            )
        |> required "maps" (D.dict mapDecoder |> D.andThen strToIntDictDecoder)
        |> required "mapPath" (D.list D.int)
        |> required "nextId" D.int
        ----- transient -----
        |> hardcoded default.selection
        |> hardcoded default.editState
        |> hardcoded default.measureText
        -- components
        |> hardcoded default.mouse
        |> hardcoded default.search
        |> hardcoded default.iconMenu


mapDecoder : D.Decoder Map
mapDecoder =
    D.map4 Map
        (D.field "id" D.int)
        (D.field "items"
            (D.dict
                (D.map5 MapItem
                    (D.field "id" D.int)
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
                                    (D.field "displayMode" D.string |> D.andThen displayModeDecoder)
                        , D.field "assocProps" <| D.succeed (MapAssoc AssocProps)
                        ]
                    )
                    (D.field "parentAssocId" D.int)
                )
                |> D.andThen strToIntDictDecoder
            )
        )
        (D.field "rect" <|
            D.map4 Rectangle
                (D.field "x1" D.float)
                (D.field "y1" D.float)
                (D.field "x2" D.float)
                (D.field "y2" D.float)
        )
        (D.field "parentMapId" D.int)


strToIntDictDecoder : Dict String v -> D.Decoder (Dict Id v)
strToIntDictDecoder strDict =
    case strToIntDict strDict of
        Just dict ->
            D.succeed dict

        Nothing ->
            D.fail "Transformation Dict String -> Int failed"


strToIntDict : Dict String v -> Maybe (Dict Id v)
strToIntDict strDict =
    strDict
        |> Dict.foldl
            (\k v b ->
                case b of
                    Just b_ ->
                        case String.toInt k of
                            Just i ->
                                Just (Dict.insert i v b_)

                            Nothing ->
                                Nothing

                    Nothing ->
                        Nothing
            )
            (Just Dict.empty)


maybeString : String -> D.Decoder (Maybe String)
maybeString str =
    D.succeed
        (case str of
            "" ->
                Nothing

            _ ->
                Just str
        )


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
