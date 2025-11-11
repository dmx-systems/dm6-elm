module ModelAPI exposing (..)

import AppModel exposing (..)
import Config as C
import Model exposing (..)
import Utils exposing (..)

import Dict
import Set
import String exposing (fromInt)
import UndoList



-- MODEL API


-- Items

topicById : Id -> Model -> Maybe TopicInfo
topicById topicId model =
  case itemById topicId model of
    Just {info} ->
      case info of
        Topic topic -> Just topic
        Assoc _ -> topicMismatch "topicById" topicId Nothing
    Nothing -> fail "topicById" topicId Nothing


assocById : Id -> Model -> Maybe AssocInfo
assocById assocId model =
  case itemById assocId model of
    Just {info} ->
      case info of
        Topic _ -> assocMismatch "assocById" assocId Nothing
        Assoc assoc -> Just assoc
    Nothing -> fail "assocById" assocId Nothing


itemById : Id -> Model -> Maybe Item
itemById itemId model =
  case model.items |> Dict.get itemId of
    Just item -> Just item
    Nothing -> illegalItemId "itemById" itemId Nothing


topicLabel : TopicInfo -> String
topicLabel topic =
  case topic.text |> String.lines |> List.head of
    Just line -> line
    Nothing -> ""


addTopic : String -> Maybe IconName -> Model -> (Model, Id)
addTopic text iconName model =
  let
    id = model.nextId
    topic = Item id (Topic <| TopicInfo id text iconName) Set.empty
  in
  ( { model | items = model.items |> Dict.insert id topic }
    |> nextId
  , id
  )


addAssoc : ItemType -> RoleType -> Id -> RoleType -> Id -> Model -> (Model, Id)
addAssoc itemType role1 player1 role2 player2 model =
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


removeItem : Id -> Model -> Model
removeItem itemId model =
  itemAssocIds itemId model |> Set.foldr
    removeItem -- recursion
    model
    |> removeAssocRefs_ itemId
    |> removeItem_ itemId


removeAssocRefs_ : Id -> Model -> Model
removeAssocRefs_ itemId model =
  case itemById itemId model of
    Just {info} ->
      case info of
        Assoc assoc ->
          model
          |> removeAssocId_ assoc.id assoc.player1
          |> removeAssocId_ assoc.id assoc.player2
        Topic _ -> model
    Nothing -> model -- error is already logged


removeItem_ : Id -> Model -> Model
removeItem_ itemId model =
  { model
    | items = model.items |> Dict.remove itemId -- delete item
    , boxes = model.boxes |> Dict.map -- delete item from all boxes
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
  case assocById assocId model of
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
  case itemById itemId model of
    Just {assocIds} -> assocIds
    Nothing -> Set.empty -- error is already logged


insertAssocId_ : Id -> Id -> Model -> Model
insertAssocId_ assocId itemId model =
  model
  |> updateItem itemId (\item -> {item | assocIds = item.assocIds |> Set.insert assocId})


removeAssocId_ : Id -> Id -> Model -> Model
removeAssocId_ assocId itemId model =
  model
  |> updateItem itemId (\item -> {item | assocIds = item.assocIds |> Set.remove assocId})


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


-- Boxes

isHome : Model -> Bool
isHome model =
  model |> activeMap |> isHomeMap


isHomeMap : Id -> Bool
isHomeMap id =
  id == 0


isFullscreen : BoxId -> Model -> Bool
isFullscreen mapId model =
  activeMap model == mapId


activeMap : Model -> BoxId
activeMap model =
  firstId model.boxPath


{-| Logs an error (and returns -1) if boxPath is empty.
-}
firstId : BoxPath -> BoxId
firstId boxPath =
  case boxPath of
    mapId :: _ -> mapId
    [] -> logError "firstId" "boxPath is empty!" -1


fromPath : BoxPath -> String
fromPath boxPath =
  boxPath |> List.map fromInt |> String.join ","


{-| Logs an error if map does not exist.
TODO: replace Boxes parameter by Model?
-}
mapByIdOrLog : BoxId -> Boxes -> Maybe Map
mapByIdOrLog mapId boxes =
  case mapById mapId boxes of
    Just map -> Just map
    Nothing -> illegalMapId "mapByIdOrLog" mapId Nothing


{-| TODO: replace Boxes parameter by Model? -}
mapById : BoxId -> Boxes -> Maybe Map
mapById mapId boxes =
  boxes |> Dict.get mapId


{-| TODO: replace Boxes parameter by Model? -}
hasMap : BoxId -> Boxes -> Bool
hasMap mapId boxes =
  boxes |> Dict.member mapId


addMap : BoxId -> Model -> Model
addMap mapId model =
  { model | boxes = model.boxes |> Dict.insert
    mapId
    (Map mapId (Rectangle 0 0 0 0) Dict.empty)
  }


updateMapRect : BoxId -> (Rectangle -> Rectangle) -> Model -> Model
updateMapRect mapId rectFunc model =
  { model | boxes = updateMaps
    mapId
    (\map ->
      { map | rect = rectFunc map.rect }
    )
    model.boxes
  }


{-| Logs an error if map does not exist or item is not in map or is not a topic.
TODO: replace Boxes parameter by Model?
-}
topicPos : Id -> BoxId -> Boxes -> Maybe Point
topicPos topicId mapId boxes =
  case topicProps topicId mapId boxes of
    Just { pos } -> Just pos
    Nothing -> fail "topicPos" {topicId = topicId, mapId = mapId} Nothing


{-| Logs an error if map does not exist or if topic is not in map -}
setTopicPos : Id -> BoxId -> Point -> Model -> Model
setTopicPos topicId mapId pos model =
  model |> updateTopicProps topicId mapId
    (\props -> { props | pos = pos })


{-| Logs an error if map does not exist or if topic is not in map -}
setTopicPosByDelta : Id -> BoxId -> Delta -> Model -> Model
setTopicPosByDelta topicId mapId delta model =
  model |> updateTopicProps topicId mapId
    (\props ->
      { props | pos =
        Point
          (props.pos.x + delta.x)
          (props.pos.y + delta.y)
      }
    )


{-| TODO: replace Boxes parameter by Model? -}
topicSize : Id -> BoxId -> Boxes -> Maybe Size
topicSize topicId mapId boxes =
  case topicProps topicId mapId boxes of
    Just { size } -> Just size
    Nothing -> fail "topicSize" {topicId = topicId, mapId = mapId} Nothing


{-| Logs an error if map does not exist or if topic is not in map -}
setTopicSize : Id -> BoxId -> Size -> Model -> Model
setTopicSize topicId mapId size model =
  model |> updateTopicProps topicId mapId
    (\props -> { props | size = size })


{-| TODO: replace Boxes parameter by Model? -}
displayMode : Id -> BoxId -> Boxes -> Maybe DisplayMode
displayMode topicId mapId boxes =
  case topicProps topicId mapId boxes of
    Just props -> Just props.displayMode
    Nothing -> fail "displayMode" {topicId = topicId, mapId = mapId} Nothing


{-| Logs an error if map does not exist or if topic is not in map -}
setDisplayMode : Id -> BoxId -> DisplayMode -> Model -> Model
setDisplayMode topicId mapId display model =
  model |> updateTopicProps topicId mapId
    (\props -> { props | displayMode = display })


{-| TODO: replace Boxes parameter by Model? -}
topicProps : Id -> BoxId -> Boxes -> Maybe TopicProps
topicProps topicId mapId boxes =
  case mapItemById topicId mapId boxes of
    Just mapItem ->
      case mapItem.props of
        MapTopic props -> Just props
        MapAssoc _ -> topicMismatch "topicProps" topicId Nothing
    Nothing -> fail "topicProps" {topicId = topicId, mapId = mapId} Nothing


{-| Logs an error if map does not exist or if topic is not in map -}
updateTopicProps : Id -> BoxId -> (TopicProps -> TopicProps) -> Model -> Model
updateTopicProps topicId mapId propsFunc model =
  { model | boxes = model.boxes |> updateMaps mapId
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


{-| Initial props for a newly revealed topic -}
initItemProps : Id -> Model -> MapProps
initItemProps itemId model =
  case itemById itemId model of
    Just item ->
      case item.info of
        Topic _ -> MapTopic <| initTopicProps itemId model
        Assoc _ -> MapAssoc {}
    Nothing -> MapAssoc {} -- error is already logged


{-| Initial props for a newly revealed topic -}
initTopicProps : Id -> Model -> TopicProps
initTopicProps topicId model =
  TopicProps
    ( Point 0 0 ) -- TODO, see also MapRenderer's viewLimboAssoc()
    C.topicSize
    ( if hasMap topicId model.boxes then
        Box BlackBox
      else
        Monad LabelOnly
    )


{-| Logs an error if map does not exist or item is not in map.
TODO: replace Boxes parameter by Model?
-}
mapItemById : Id -> BoxId -> Boxes -> Maybe MapItem
mapItemById itemId mapId boxes =
  mapByIdOrLog mapId boxes |> Maybe.andThen
    (\map ->
      case map.items |> Dict.get itemId of
        Just mapItem -> Just mapItem
        Nothing -> itemNotInMap "mapItemById" itemId map.id Nothing
    )


isItemInMapDeep : Id -> BoxId -> Model -> Bool
isItemInMapDeep itemId mapId model =
  if itemId == mapId then
    True
  else
    case mapById mapId model.boxes of
      Just map -> map.items |> Dict.keys |> List.any
        (\id -> isItemInMapDeep itemId id model)
      Nothing -> False


{-| Logs an error if map does not exist. -}
isItemInMap : Id -> BoxId -> Model -> Bool
isItemInMap itemId mapId model =
  case mapByIdOrLog mapId model.boxes of
    Just map -> map.items |> Dict.member itemId
    Nothing -> False


{-| Adds an item to a map and creates a connecting association.
Presumption: the item is not yet contained in the map. Otherwise the existing map-item would be
overridden and another association still be created. This is not what you want.
Can be used for both, topics and associations.
-}
putItemOnMap : Id -> MapProps -> BoxId -> Model -> Model
putItemOnMap itemId props mapId model =
  let
    (newModel, parentAssocId) = addAssoc
      "dmx.composition"
      "dmx.child" itemId
      "dmx.parent" mapId
      model
    mapItem = MapItem itemId parentAssocId False False props -- hidden=False, pinned=False
    _ = info "putItemOnMap"
      { itemId = itemId, parentAssocId = parentAssocId, props = props, mapId = mapId}
  in
  { newModel | boxes = newModel.boxes |> updateMaps
      mapId
      (\map -> { map | items = map.items |> Dict.insert itemId mapItem })
  }


{-| Presumption: the item *is* contained in the map. Sets its "hidden" flag to False.
Can be used for both, topics and associations.
If the item is *not* contained in the map, or its "hidden" flag is False already, its a no-op.
Logs an error if map does not exist.
-}
showItem : Id -> BoxId -> Model -> Model
showItem itemId mapId model =
  { model | boxes = model.boxes |> updateMaps
    mapId
    (\map ->
      { map | items = map.items |> Dict.update itemId
        (\maybeItem ->
          case maybeItem of
            Just mapItem -> Just { mapItem | hidden = False }
            Nothing -> Nothing
        )
      }
    )
  }


hideItem : Id -> BoxId -> Model -> Model
hideItem itemId mapId model =
  { model | boxes = model.boxes |> updateMaps
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


{-| Logs an error if map does not exist.
TODO: replace Boxes parameter by Model?
-}
updateMaps : BoxId -> (Map -> Map) -> Boxes -> Boxes
updateMaps mapId mapFunc boxes =
  boxes |> Dict.update mapId
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
  case assocById assocId model of
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


{-| useful as a filter predicate
-}
isVisible : MapItem -> Bool
isVisible item =
  not item.hidden


-- Selection

select : Id -> BoxPath -> Model -> Model
select itemId boxPath model =
  let
    _ = info "select" (itemId, boxPath)
  in
  { model | selection = [ (itemId, boxPath) ] }


resetSelection : Model -> Model
resetSelection model =
  { model | selection = [] }


isSelected : Id -> BoxId -> Model -> Bool
isSelected itemId mapId model =
  model.selection |> List.any
    (\(id, boxPath) ->
      case boxPath of
        mapId_ :: _ -> itemId == id && mapId == mapId_
        [] -> False
    )


singleSelection : Model -> Maybe (Id, BoxPath)
singleSelection model =
  case model.selection of
    [ selItem ] -> Just selItem
    _ -> Nothing


singleSelectionMapId : Model -> Maybe BoxId
singleSelectionMapId model =
  case singleSelection model of
    Just (_, boxPath) ->
      case boxPath of
        mapId :: _ -> Just mapId
        [] -> Nothing
    Nothing -> Nothing


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
