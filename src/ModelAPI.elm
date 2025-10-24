module ModelAPI exposing (..)

import AppModel exposing (..)
import Config exposing (..)
import Model exposing (..)
import Utils exposing (..)

import Dict
import Set
import String exposing (fromInt)
import UndoList



-- MODEL API


-- Items

-- TODO: rename to "topicById"
getTopicInfo : Id -> Model -> Maybe TopicInfo
getTopicInfo topicId model =
  case getItem topicId model of
    Just {info} ->
      case info of
        Topic topic -> Just topic
        Assoc _ -> topicMismatch "getTopicInfo" topicId Nothing
    Nothing -> fail "getTopicInfo" topicId Nothing


-- TODO: rename to "assocById"
getAssocInfo : Id -> Model -> Maybe AssocInfo
getAssocInfo assocId model =
  case getItem assocId model of
    Just {info} ->
      case info of
        Topic _ -> assocMismatch "getAssocInfo" assocId Nothing
        Assoc assoc -> Just assoc
    Nothing -> fail "getAssocInfo" assocId Nothing


-- TODO: rename to "itemById"
getItem : Id -> Model -> Maybe Item
getItem itemId model =
  case model.items |> Dict.get itemId of
    Just item -> Just item
    Nothing -> illegalItemId "getItem" itemId Nothing


getTopicLabel : TopicInfo -> String
getTopicLabel topic =
  case topic.text |> String.lines |> List.head of
    Just line -> line
    Nothing -> ""


createTopic : String -> Maybe IconName -> Model -> (Model, Id)
createTopic text iconName model =
  let
    id = model.nextId
    topic = Item id (Topic <| TopicInfo id text iconName) Set.empty
  in
  ( { model | items = model.items |> Dict.insert id topic }
    |> nextId
  , id
  )


createAssoc : ItemType -> RoleType -> Id -> RoleType -> Id -> Model -> (Model, Id)
createAssoc itemType role1 player1 role2 player2 model =
  let
    id = model.nextId
    assoc = Item id (Assoc <| AssocInfo id itemType role1 player1 role2 player2) Set.empty
  in
  ( { model | items = model.items |> Dict.insert id assoc }
    |> insertAssocId_ id player1
    |> insertAssocId_ id player2
    |> nextId
  , id
  )


deleteItem : Id -> Model -> Model
deleteItem itemId model =
  itemAssocIds itemId model |> Set.foldr
    deleteItem -- recursion
    model
    |> removeAssocRefs_ itemId
    |> deleteItem_ itemId


removeAssocRefs_ : Id -> Model -> Model
removeAssocRefs_ itemId model =
  case getItem itemId model of
    Just {info} ->
      case info of
        Assoc assoc ->
          model
          |> removeAssocId_ assoc.id assoc.player1
          |> removeAssocId_ assoc.id assoc.player2
        Topic _ -> model
    Nothing -> model -- error is already logged


deleteItem_ : Id -> Model -> Model
deleteItem_ itemId model =
  { model
    | items = model.items |> Dict.remove itemId -- delete item
    , maps = model.maps |> Dict.map -- delete item from all maps
      (\_ map -> { map | items = map.items |> Dict.remove itemId })
  }


relatedItems : Id -> Model -> List (Id, Id)
relatedItems itemId model =
  itemAssocIds itemId model |> Set.foldr
    (\assocId relItemsAcc ->
      (otherPlayerId assocId itemId model, assocId) :: relItemsAcc
    )
    []


otherPlayerId : Id -> Id -> Model -> Id
otherPlayerId assocId playerId model =
  case getAssocInfo assocId model of
    Just {player1, player2} ->
      if playerId == player1 then
        player2
      else if playerId == player2 then
        player1
      else
        logError "otherPlayerId"
          (fromInt playerId ++ " is not a player in assoc " ++ fromInt assocId) -1
    Nothing -> -1 -- error is already logged


itemAssocIds : Id -> Model -> AssocIds
itemAssocIds itemId model =
  case getItem itemId model of
    Just {assocIds} -> assocIds
    Nothing -> Set.empty -- error is already logged


insertAssocId_ : Id -> Id -> Model -> Model
insertAssocId_ assocId itemId model =
  case not <| isHomeMap itemId of -- FIXME: add topic 0 and remove this condition
    True ->
      model
      |> updateItem itemId (\item -> {item | assocIds = item.assocIds |> Set.insert assocId})
    False -> model


removeAssocId_ : Id -> Id -> Model -> Model
removeAssocId_ assocId itemId model =
  case not <| isHomeMap itemId of
    True ->
      model
      |> updateItem itemId (\item -> {item | assocIds = item.assocIds |> Set.remove assocId})
    False -> model


updateTopicInfo : Id -> (TopicInfo -> TopicInfo) -> Model -> Model
updateTopicInfo topicId topicFunc model =
  model |> updateItem topicId
    (\item ->
      case item.info of
        Topic topic -> { item | info = Topic <| topicFunc topic }
        Assoc _  -> topicMismatch "updateTopicInfo" topicId item
    )


updateItem : Id -> (Item -> Item) -> Model -> Model
updateItem itemId itemFunc model =
  { model | items = model.items |> Dict.update itemId
    (\maybeItem ->
      case maybeItem of
        Just item -> Just <| itemFunc item
        Nothing -> illegalItemId "updateItem" itemId Nothing
    )
  }


nextId : Model -> Model
nextId model =
  { model | nextId = model.nextId + 1 }


-- Maps

isHome : Model -> Bool
isHome model =
  model |> activeMap |> isHomeMap


isHomeMap : Id -> Bool
isHomeMap id =
  id == 0


isFullscreen : MapId -> Model -> Bool
isFullscreen mapId model =
  activeMap model == mapId


activeMap : Model -> MapId
activeMap model =
  case List.head model.mapPath of
    Just mapId -> mapId
    Nothing -> logError "activeMap" "mapPath is empty!" 0


{-| Returns -1 if mapPath is empty -}
getMapId : MapPath -> MapId
getMapId mapPath =
  case mapPath of
    mapId :: _ -> mapId
    _ -> -1


fromPath : MapPath -> String
fromPath mapPath =
  mapPath |> List.map fromInt |> String.join ","


{-| Logs an error if map does not exist -}
getMap : MapId -> Maps -> Maybe Map
getMap mapId maps =
  case getMapIfExists mapId maps of
    Just map -> Just map
    Nothing -> illegalMapId "getMap" mapId Nothing


getMapIfExists : MapId -> Maps -> Maybe Map
getMapIfExists mapId maps =
  maps |> Dict.get mapId


hasMap : MapId -> Maps -> Bool
hasMap mapId maps =
  maps |> Dict.member mapId


createMap : MapId -> Model -> Model
createMap mapId model =
  { model | maps = model.maps |> Dict.insert
    mapId
    (Map mapId (Rectangle 0 0 0 0) Dict.empty)
  }


updateMapRect : MapId -> (Rectangle -> Rectangle) -> Model -> Model
updateMapRect mapId rectFunc model =
  { model | maps = updateMaps
    mapId
    (\map ->
      { map | rect = rectFunc map.rect }
    )
    model.maps
  }


{-| Logs an error if map does not exist or item is not in map or is not a topic -}
getTopicPos : Id -> MapId -> Maps -> Maybe Point
getTopicPos topicId mapId maps =
  case getTopicProps topicId mapId maps of
    Just { pos } -> Just pos
    Nothing -> fail "getTopicPos" {topicId = topicId, mapId = mapId} Nothing


{-| Logs an error if map does not exist or if topic is not in map -}
setTopicPos : Id -> MapId -> Point -> Model -> Model
setTopicPos topicId mapId pos model =
  model |> updateTopicProps topicId mapId
    (\props -> { props | pos = pos })


{-| Logs an error if map does not exist or if topic is not in map -}
setTopicPosByDelta : Id -> MapId -> Delta -> Model -> Model
setTopicPosByDelta topicId mapId delta model =
  model |> updateTopicProps topicId mapId
    (\props ->
      { props | pos =
        Point
          (props.pos.x + delta.x)
          (props.pos.y + delta.y)
      }
    )


getTopicSize : Id -> MapId -> Maps -> Maybe Size
getTopicSize topicId mapId maps =
  case getTopicProps topicId mapId maps of
    Just { size } -> Just size
    Nothing -> fail "getTopicSize" {topicId = topicId, mapId = mapId} Nothing


{-| Logs an error if map does not exist or if topic is not in map -}
setTopicSize : Id -> MapId -> Size -> Model -> Model
setTopicSize topicId mapId size model =
  model |> updateTopicProps topicId mapId
    (\props -> { props | size = size })


getDisplayMode : Id -> MapId -> Maps -> Maybe DisplayMode
getDisplayMode topicId mapId maps =
  case getTopicProps topicId mapId maps of
    Just { displayMode } -> Just displayMode
    Nothing -> fail "getDisplayMode" {topicId = topicId, mapId = mapId} Nothing


{-| Logs an error if map does not exist or if topic is not in map -}
setDisplayMode : Id -> MapId -> DisplayMode -> Model -> Model
setDisplayMode topicId mapId displayMode model =
  model |> updateTopicProps topicId mapId
    (\props -> { props | displayMode = displayMode })


getTopicProps : Id -> MapId -> Maps -> Maybe TopicProps
getTopicProps topicId mapId maps =
  case getMapItemById topicId mapId maps of
    Just mapItem ->
      case mapItem.props of
        MapTopic props -> Just props
        MapAssoc _ -> topicMismatch "getTopicProps" topicId Nothing
    Nothing -> fail "getTopicProps" {topicId = topicId, mapId = mapId} Nothing


{-| Logs an error if map does not exist or if topic is not in map -}
updateTopicProps : Id -> MapId -> (TopicProps -> TopicProps) -> Model -> Model
updateTopicProps topicId mapId propsFunc model =
  { model | maps = model.maps |> updateMaps mapId
    (\map ->
      { map | items = map.items |> Dict.update topicId
        (\mapItem_ ->
          case mapItem_ of
            Just mapItem ->
              case mapItem.props of
                MapTopic props -> Just
                  { mapItem | props = MapTopic (propsFunc props) }
                MapAssoc _ -> topicMismatch "updateTopicProps" topicId Nothing
            Nothing -> illegalItemId "updateTopicProps" topicId Nothing
        )
      }
    )
  }


{-| Useful when revealing an existing topic -}
defaultProps : Id -> Size -> Model -> TopicProps
defaultProps topicId size model =
  TopicProps
    ( Point 0 0 ) -- TODO
    size
    ( if hasMap topicId model.maps then
        Container BlackBox
      else
        Monad LabelOnly
    )


{-| Logs an error if map does not exist or item is not in map -}
getMapItemById : Id -> MapId -> Maps -> Maybe MapItem
getMapItemById itemId mapId maps =
  getMap mapId maps |> Maybe.andThen (getMapItem itemId)


{-| Logs an error if item is not in map -}
getMapItem : Id -> Map -> Maybe MapItem
getMapItem itemId map =
  case map.items |> Dict.get itemId of
    Just mapItem -> Just mapItem
    Nothing -> itemNotInMap "getMapItem" itemId map.id Nothing


{-| Logs an error if map does not exist -}
isItemInMap : Id -> MapId -> Model -> Bool
isItemInMap itemId mapId model =
  case getMap mapId model.maps of
    Just map ->
      case map.items |> Dict.get itemId of
        Just _ -> True
        Nothing -> False
    Nothing -> False


createTopicIn : String -> Maybe IconName -> MapPath -> Model -> Model
createTopicIn text iconName mapPath model =
  let
    mapId = getMapId mapPath
  in
  case getMap mapId model.maps of
    Just map ->
      let
        (newModel, topicId) = createTopic text iconName model
        props = MapTopic <| TopicProps
          (Point
            (newTopicPos.x + map.rect.x1)
            (newTopicPos.y + map.rect.y1)
          )
          topicDetailSize
          (Monad LabelOnly)
      in
      newModel
      |> addItemToMap topicId props mapId
      |> select topicId mapPath
    Nothing -> model


-- Presumption: both players exist in same map
createDefaultAssocIn : Id -> Id -> MapId -> Model -> Model
createDefaultAssocIn player1 player2 mapId model =
  createAssocIn
    "dmx.association"
    "dmx.default" player1
    "dmx.default" player2
    mapId model


-- Presumption: both players exist in same map
createAssocIn : ItemType -> RoleType -> Id -> RoleType -> Id -> MapId -> Model -> Model
createAssocIn itemType role1 player1 role2 player2 mapId model =
  let
    (newModel, assocId) = createAssoc itemType role1 player1 role2 player2 model
    props = MapAssoc AssocProps
  in
  addItemToMap assocId props mapId newModel


{-| Precondition: the item is not yet contained in the map
-}
addItemToMap : Id -> MapProps -> MapId -> Model -> Model
addItemToMap itemId props mapId model =
  let
    (newModel, parentAssocId) = createAssoc
      "dmx.composition"
      "dmx.child" itemId
      "dmx.parent" mapId
      model
    mapItem = MapItem itemId parentAssocId False False props -- hidden=False, pinned=False
    _ = info "addItemToMap"
      { itemId = itemId, parentAssocId = parentAssocId, props = props, mapId = mapId}
  in
  { newModel | maps =
    updateMaps
      mapId
      (\map -> { map | items = map.items |> Dict.insert itemId mapItem })
      newModel.maps
  }


showItem : Id -> MapId -> Model -> Model
showItem itemId mapId model =
  { model | maps = model.maps |> updateMaps
    mapId
    (\map ->
      { map | items = Dict.update itemId
        (\maybeItem ->
          case maybeItem of
            Just mapItem -> Just { mapItem | hidden = False }
            Nothing -> Nothing
        )
        map.items
      }
    )
  }


hideItem : Id -> MapId -> Model -> Model
hideItem itemId mapId model =
  { model | maps = model.maps |> updateMaps
    mapId
    (\map -> { map | items = hideItem_ itemId map.items model })
  }


hideItem_ : Id -> MapItems -> Model -> MapItems
hideItem_ itemId items model =
  mapAssocsOfPlayer_ itemId items model |> List.foldr
    (\assocId itemsAcc -> hideItem_ assocId itemsAcc model)
    (items |> Dict.update
      itemId
      (\item_ ->
        case item_ of
          Just item -> Just { item | hidden = True }
          Nothing -> Nothing
      )
    )


{-| Logs an error if map does not exist -}
updateMaps : MapId -> (Map -> Map) -> Maps -> Maps
updateMaps mapId mapFunc maps =
  maps |> Dict.update mapId
    (\map_ ->
      case map_ of
        Just map -> Just (mapFunc map)
        Nothing -> illegalMapId "updateMaps" mapId Nothing
    )


mapAssocsOfPlayer_ : Id -> MapItems -> Model -> List Id
mapAssocsOfPlayer_ playerId items model =
  items |> Dict.values
    |> List.filter isMapAssoc
    |> List.map .id
    |> List.filter (hasPlayer playerId model)


hasPlayer : Id -> Model -> Id -> Bool
hasPlayer playerId model assocId =
  case getAssocInfo assocId model of
    Just assoc -> assoc.player1 == playerId || assoc.player2 == playerId
    Nothing -> False


{-| useful as a filter predicate
-}
isTopic : Item -> Bool
isTopic item =
  case item.info of
    Topic _ -> True
    Assoc _ -> False


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
    MapTopic _ -> True
    MapAssoc _ -> False


{-| useful as a filter predicate
-}
isMapAssoc : MapItem -> Bool
isMapAssoc item =
  not (isMapTopic item)


isVisible : MapItem -> Bool
isVisible item =
  not item.hidden


-- Selection

select : Id -> MapPath -> Model -> Model
select itemId mapPath model =
  { model | selection = [ (itemId, mapPath) ] }


resetSelection : Model -> Model
resetSelection model =
  { model | selection = [] }


isSelected : Id -> MapId -> Model -> Bool
isSelected itemId mapId model =
  model.selection |> List.any
    (\(id, mapPath) ->
      case mapPath of
        mapId_ :: _ -> itemId == id && mapId == mapId_
        [] -> False
    )


singleSelection : Model -> Maybe (Id, MapPath)
singleSelection model =
  case model.selection of
    [ selItem ] -> Just selItem
    _ -> Nothing


-- Undo / Redo

push : UndoModel -> (Model, Cmd Msg) -> (UndoModel, Cmd Msg)
push undoModel (model, cmd) =
  (UndoList.new model undoModel, cmd)


swap : UndoModel -> (Model, Cmd Msg) -> (UndoModel, Cmd Msg)
swap undoModel (model, cmd) =
  (UndoList.mapPresent (\_ -> model) undoModel, cmd)


reset : (Model, Cmd Msg) -> (UndoModel, Cmd Msg)
reset (model, cmd) =
  (UndoList.fresh model, cmd)



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
