module ModelAPI exposing (..)

import AppModel as AM
import Config exposing (..)
import Dict
import Domain.Reparent as R
import Json.Decode as D
import Model exposing (..)
import String exposing (fromInt)
import Utils exposing (..)



-- MODEL API
-- Items


getTopicInfo : Id -> AM.Model -> Maybe TopicInfo
getTopicInfo topicId model =
    case model.items |> Dict.get topicId of
        Just { info } ->
            case info of
                Topic topic ->
                    Just topic

                Assoc _ ->
                    topicMismatch "getTopicInfo" topicId Nothing

        Nothing ->
            illegalItemId "getTopicInfo" topicId Nothing


getAssocInfo : Id -> AM.Model -> Maybe AssocInfo
getAssocInfo assocId model =
    case model.items |> Dict.get assocId of
        Just { info } ->
            case info of
                Topic _ ->
                    assocMismatch "getAssocInfo" assocId Nothing

                Assoc assoc ->
                    Just assoc

        Nothing ->
            illegalItemId "getAssocInfo" assocId Nothing


updateTopicInfo : Id -> (TopicInfo -> TopicInfo) -> AM.Model -> AM.Model
updateTopicInfo topicId topicFunc model =
    { model
        | items =
            model.items
                |> Dict.update topicId
                    (\maybeItem ->
                        case maybeItem of
                            Just item ->
                                case item.info of
                                    Topic topic ->
                                        Just { item | info = topicFunc topic |> Topic }

                                    Assoc _ ->
                                        topicMismatch "updateTopicInfo" topicId Nothing

                            Nothing ->
                                illegalItemId "updateTopicInfo" topicId Nothing
                    )
    }


getTopicLabel : TopicInfo -> String
getTopicLabel topic =
    case topic.text |> String.lines |> List.head of
        Just line ->
            line

        Nothing ->
            ""


createTopic : String -> Maybe IconName -> AM.Model -> ( AM.Model, Id )
createTopic text iconName model =
    let
        id =
            model.nextId

        topic =
            Item id <| Topic <| TopicInfo id text iconName
    in
    ( { model | items = model.items |> Dict.insert id topic }
        |> nextId
    , id
    )


createAssoc : ItemType -> RoleType -> Id -> RoleType -> Id -> AM.Model -> ( AM.Model, Id )
createAssoc itemType role1 player1 role2 player2 model =
    let
        id =
            model.nextId

        assoc =
            Item id <| Assoc <| AssocInfo id itemType role1 player1 role2 player2
    in
    ( { model | items = model.items |> Dict.insert id assoc }
        |> nextId
    , id
    )


nextId : AM.Model -> AM.Model
nextId model =
    { model | nextId = model.nextId + 1 }



-- Maps


isHome : AM.Model -> Bool
isHome model =
    activeMap model == 0


isFullscreen : MapId -> AM.Model -> Bool
isFullscreen mapId model =
    activeMap model == mapId


activeMap : AM.Model -> MapId
activeMap model =
    case List.head model.mapPath of
        Just mapId ->
            mapId

        Nothing ->
            logError "activeMap" "mapPath is empty!" 0


{-| Returns -1 if mapPath is empty
-}
getMapId : MapPath -> MapId
getMapId mapPath =
    case mapPath of
        mapId :: _ ->
            mapId

        _ ->
            -1


fromPath : MapPath -> String
fromPath mapPath =
    mapPath |> List.map fromInt |> String.join ","


{-| Logs an error if map does not exist
-}
getMap : MapId -> Maps -> Maybe Map
getMap mapId maps =
    case getMapIfExists mapId maps of
        Just map ->
            Just map

        Nothing ->
            illegalMapId "getMap" mapId Nothing


getMapIfExists : MapId -> Maps -> Maybe Map
getMapIfExists mapId maps =
    maps |> Dict.get mapId


hasMap : MapId -> Maps -> Bool
hasMap mapId maps =
    maps |> Dict.member mapId


createMap : MapId -> AM.Model -> AM.Model
createMap mapId model =
    { model
        | maps =
            model.maps
                |> Dict.insert
                    mapId
                    (Map mapId (Rectangle 0 0 0 0) Dict.empty)
    }


updateMapRect : MapId -> (Rectangle -> Rectangle) -> AM.Model -> AM.Model
updateMapRect mapId rectFunc model =
    { model
        | maps =
            updateMaps
                mapId
                (\map ->
                    { map | rect = rectFunc map.rect }
                )
                model.maps
    }


{-| Logs an error if map does not exist or item is not in map or is not a topic
-}
getTopicPos : Id -> MapId -> Maps -> Maybe Point
getTopicPos topicId mapId maps =
    case getTopicProps topicId mapId maps of
        Just { pos } ->
            Just pos

        Nothing ->
            fail "getTopicPos" { topicId = topicId, mapId = mapId } Nothing


{-| Logs an error if topic is not in map
-}
setTopicPos : Id -> MapId -> Point -> AM.Model -> AM.Model
setTopicPos topicId mapId pos model =
    model
        |> updateTopicProps topicId
            mapId
            (\props -> { props | pos = pos })


{-| Logs an error if topic is not in map
-}
setTopicPosByDelta : Id -> MapId -> Delta -> AM.Model -> AM.Model
setTopicPosByDelta topicId mapId delta model =
    model
        |> updateTopicProps topicId
            mapId
            (\props ->
                { props
                    | pos =
                        Point
                            (props.pos.x + delta.x)
                            (props.pos.y + delta.y)
                }
            )


getTopicSize : Id -> MapId -> Maps -> Maybe Size
getTopicSize topicId mapId maps =
    case getTopicProps topicId mapId maps of
        Just { size } ->
            Just size

        Nothing ->
            fail "getTopicSize" { topicId = topicId, mapId = mapId } Nothing


{-| Logs an error if topic is not in map
-}
setTopicSize : Id -> MapId -> Size -> AM.Model -> AM.Model
setTopicSize topicId mapId size model =
    model
        |> updateTopicProps topicId
            mapId
            (\props -> { props | size = size })


getDisplayMode : Id -> MapId -> Maps -> Maybe DisplayMode
getDisplayMode topicId mapId maps =
    case getTopicProps topicId mapId maps of
        Just { displayMode } ->
            Just displayMode

        Nothing ->
            fail "getDisplayMode" { topicId = topicId, mapId = mapId } Nothing


{-| Logs an error if topic is not in map
-}
setDisplayMode : Id -> MapId -> DisplayMode -> AM.Model -> AM.Model
setDisplayMode topicId mapId displayMode model =
    model
        |> updateTopicProps topicId
            mapId
            (\props -> { props | displayMode = displayMode })


getTopicProps : Id -> MapId -> Maps -> Maybe TopicProps
getTopicProps topicId mapId maps =
    case getMapItemById topicId mapId maps of
        Just mapItem ->
            case mapItem.props of
                MapTopic props ->
                    Just props

                MapAssoc _ ->
                    topicMismatch "getTopicProps" topicId Nothing

        Nothing ->
            fail "getTopicProps" { topicId = topicId, mapId = mapId } Nothing


{-| Logs an error if topic is not in map
-}
updateTopicProps : Id -> MapId -> (TopicProps -> TopicProps) -> AM.Model -> AM.Model
updateTopicProps topicId mapId propsFunc model =
    { model
        | maps =
            model.maps
                |> updateMaps mapId
                    (\map ->
                        { map
                            | items =
                                map.items
                                    |> Dict.update topicId
                                        (\mapItem_ ->
                                            case mapItem_ of
                                                Just mapItem ->
                                                    case mapItem.props of
                                                        MapTopic props ->
                                                            Just
                                                                { mapItem | props = MapTopic (propsFunc props) }

                                                        MapAssoc _ ->
                                                            topicMismatch "updateTopicProps" topicId Nothing

                                                Nothing ->
                                                    illegalItemId "updateTopicProps" topicId Nothing
                                        )
                        }
                    )
    }


{-| Useful when revealing an existing topic
-}
defaultProps : Id -> Size -> AM.Model -> TopicProps
defaultProps topicId size model =
    TopicProps
        (Point 0 0)
        -- TODO
        size
        (if hasMap topicId model.maps then
            Container BlackBox

         else
            Monad LabelOnly
        )


{-| Logs an error if map does not exist or item is not in map
-}
getMapItemById : Id -> MapId -> Maps -> Maybe MapItem
getMapItemById itemId mapId maps =
    getMap mapId maps |> Maybe.andThen (getMapItem itemId)


{-| Logs an error if item is not in map
-}
getMapItem : Id -> Map -> Maybe MapItem
getMapItem itemId map =
    case map.items |> Dict.get itemId of
        Just mapItem ->
            Just mapItem

        Nothing ->
            itemNotInMap "getMapItem" itemId map.id Nothing


{-| Logs an error if map does not exist
-}
isItemInMap : Id -> MapId -> AM.Model -> Bool
isItemInMap itemId mapId model =
    case getMap mapId model.maps of
        Just map ->
            case map.items |> Dict.get itemId of
                Just _ ->
                    True

                Nothing ->
                    False

        Nothing ->
            False


createTopicAndAddToMap : String -> Maybe IconName -> MapId -> AM.Model -> AM.Model
createTopicAndAddToMap text iconName mapId model =
    case getMap mapId model.maps of
        Just map ->
            let
                ( newModel, topicId ) =
                    createTopic text iconName model

                props =
                    MapTopic <|
                        TopicProps
                            (Point
                                (newTopicPos.x + map.rect.x1)
                                (newTopicPos.y + map.rect.y1)
                            )
                            topicDetailSize
                            (Monad LabelOnly)
            in
            newModel
                |> addItemToMap topicId props mapId
                |> select topicId mapId

        Nothing ->
            model



-- Presumption: both players exist in same map


createDefaultAssoc : Id -> Id -> MapId -> AM.Model -> AM.Model
createDefaultAssoc player1 player2 mapId model =
    createAssocAndAddToMap
        "dmx.association"
        "dmx.default"
        player1
        "dmx.default"
        player2
        mapId
        model



-- Presumption: both players exist in same map


createAssocAndAddToMap : ItemType -> RoleType -> Id -> RoleType -> Id -> MapId -> AM.Model -> AM.Model
createAssocAndAddToMap itemType role1 player1 role2 player2 mapId model =
    let
        ( newModel, assocId ) =
            createAssoc itemType role1 player1 role2 player2 model

        props =
            MapAssoc AssocProps
    in
    addItemToMap assocId props mapId newModel



-- Prevent self/descendant cycles before creating the assoc and map item.
-- Precondition: the item is not yet contained in the map


addItemToMap : Id -> MapProps -> MapId -> AM.Model -> AM.Model
addItemToMap itemId props mapId model =
    case R.canReparent itemId (Just mapId) (parentsOf model) of
        Err reason ->
            -- refuse illegal containment; leave model unchanged
            let
                _ =
                    info "addItemToMap (blocked)"
                        { itemId = itemId
                        , mapId = mapId
                        , reason = reason
                        }
            in
            model

        Ok _ ->
            let
                ( newModel, parentAssocId ) =
                    createAssoc
                        "dmx.composition"
                        "dmx.child"
                        itemId
                        "dmx.parent"
                        mapId
                        model

                mapItem =
                    MapItem itemId parentAssocId False False props

                -- hidden=False, pinned=False
                _ =
                    info "addItemToMap"
                        { itemId = itemId
                        , parentAssocId = parentAssocId
                        , props = props
                        , mapId = mapId
                        }
            in
            { newModel
                | maps =
                    updateMaps
                        mapId
                        (\m -> { m | items = Dict.insert itemId mapItem m.items })
                        newModel.maps
            }



-- Direct parents of a child via dmx.composition assocs
-- (used by Domain.Reparent to compute ancestry)


parentsOf : AM.Model -> Id -> List MapId
parentsOf model childId =
    model.items
        |> Dict.values
        |> List.filterMap
            (\item ->
                case item.info of
                    Assoc assoc ->
                        if
                            assoc.itemType
                                == "dmx.composition"
                                && assoc.role1
                                == "dmx.child"
                                && assoc.role2
                                == "dmx.parent"
                                && assoc.player1
                                == childId
                        then
                            Just assoc.player2

                        else
                            Nothing

                    _ ->
                        Nothing
            )


showItem : Id -> MapId -> AM.Model -> AM.Model
showItem itemId mapId model =
    { model
        | maps =
            model.maps
                |> updateMaps
                    mapId
                    (\map ->
                        { map
                            | items =
                                Dict.update itemId
                                    (\maybeItem ->
                                        case maybeItem of
                                            Just mapItem ->
                                                Just { mapItem | hidden = False }

                                            Nothing ->
                                                Nothing
                                    )
                                    map.items
                        }
                    )
    }


hideItem : Id -> MapId -> AM.Model -> AM.Model
hideItem itemId mapId model =
    { model
        | maps =
            model.maps
                |> updateMaps
                    mapId
                    (\map -> { map | items = hideItem_ itemId map.items model })
    }


hideItem_ : Id -> MapItems -> AM.Model -> MapItems
hideItem_ itemId items model =
    mapAssocsOfPlayer_ itemId items model
        |> List.foldr
            (\assocId itemsAcc -> hideItem_ assocId itemsAcc model)
            (items
                |> Dict.update
                    itemId
                    (\item_ ->
                        case item_ of
                            Just item ->
                                Just { item | hidden = True }

                            Nothing ->
                                Nothing
                    )
            )


updateMaps : MapId -> (Map -> Map) -> Maps -> Maps
updateMaps mapId mapFunc maps =
    maps
        |> Dict.update mapId
            (\map_ ->
                case map_ of
                    Just map ->
                        Just (mapFunc map)

                    Nothing ->
                        illegalMapId "updateMaps" mapId Nothing
            )


deleteItem : Id -> AM.Model -> AM.Model
deleteItem itemId model =
    assocsOfPlayer itemId model
        |> List.foldr
            deleteItem
            -- recursion
            { model
                | items = model.items |> Dict.remove itemId -- delete item
                , maps =
                    model.maps
                        |> Dict.map
                            -- delete item from all maps
                            (\_ map -> { map | items = map.items |> Dict.remove itemId })
            }


assocsOfPlayer : Id -> AM.Model -> List Id
assocsOfPlayer playerId model =
    model.items
        |> Dict.values
        |> List.filter isAssoc
        |> List.map .id
        |> List.filter (hasPlayer playerId model)


mapAssocsOfPlayer_ : Id -> MapItems -> AM.Model -> List Id
mapAssocsOfPlayer_ playerId items model =
    items
        |> Dict.values
        |> List.filter isMapAssoc
        |> List.map .id
        |> List.filter (hasPlayer playerId model)


hasPlayer : Id -> AM.Model -> Id -> Bool
hasPlayer playerId model assocId =
    case getAssocInfo assocId model of
        Just assoc ->
            assoc.player1 == playerId || assoc.player2 == playerId

        Nothing ->
            False


{-| useful as a filter predicate
-}
isTopic : Item -> Bool
isTopic item =
    case item.info of
        Topic _ ->
            True

        Assoc _ ->
            False


{-| useful as a filter predicate
-}
isAssoc : Item -> Bool
isAssoc item =
    not (isTopic item)


{-| useful as a filter predicate
-}
isMapTopic : MapItem -> Bool
isMapTopic item =
    case item.props of
        MapTopic _ ->
            True

        MapAssoc _ ->
            False


{-| useful as a filter predicate
-}
isMapAssoc : MapItem -> Bool
isMapAssoc item =
    not (isMapTopic item)


isVisible : MapItem -> Bool
isVisible item =
    not item.hidden



-- Selection


select : Id -> MapId -> AM.Model -> AM.Model
select id mapId model =
    { model | selection = [ ( id, mapId ) ] }


getSingleSelection : AM.Model -> Maybe ( Id, MapId )
getSingleSelection model =
    case model.selection of
        [ selItem ] ->
            Just selItem

        _ ->
            Nothing



-- Decoder


idDecoder : String -> D.Decoder Id
idDecoder str =
    case String.toInt str of
        Just int ->
            D.succeed int

        Nothing ->
            D.fail <| "\"" ++ str ++ "\" is a malformed ID"


pathDecoder : String -> D.Decoder MapPath
pathDecoder str =
    D.succeed
        (str
            |> String.split ","
            |> List.map
                (\mapIdStr ->
                    case mapIdStr |> String.toInt of
                        Just mapId ->
                            mapId

                        Nothing ->
                            logError "pathDecoder" ("\"" ++ mapIdStr ++ "\" is a malformed ID") -1
                )
        )



-- DEBUG


itemNotInMap : String -> Id -> Id -> a -> a
itemNotInMap funcName itemId mapId val =
    logError funcName ("item " ++ fromInt itemId ++ " not in map " ++ fromInt mapId) val


topicMismatch : String -> Id -> a -> a
topicMismatch funcName id val =
    logError funcName (fromInt id ++ " is not a Topic but an Assoc") val


assocMismatch : String -> Id -> a -> a
assocMismatch funcName id val =
    logError funcName (fromInt id ++ " is not an Assoc but a Topic") val


illegalMapId : String -> Id -> a -> a
illegalMapId funcName id val =
    illegalId funcName "Map" id val


illegalItemId : String -> Id -> a -> a
illegalItemId funcName id val =
    illegalId funcName "Item" id val


illegalId : String -> String -> Id -> a -> a
illegalId funcName item id val =
    logError funcName (fromInt id ++ " is an illegal " ++ item ++ " ID") val
