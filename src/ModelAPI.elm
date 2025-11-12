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
      (\_ box -> { box | items = box.items |> Dict.remove itemId })
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
isFullscreen boxId model =
  activeMap model == boxId


activeMap : Model -> BoxId
activeMap model =
  firstId model.boxPath


{-| Logs an error (and returns -1) if boxPath is empty.
-}
firstId : BoxPath -> BoxId
firstId boxPath =
  case boxPath of
    boxId :: _ -> boxId
    [] -> logError "firstId" "boxPath is empty!" -1


fromPath : BoxPath -> String
fromPath boxPath =
  boxPath |> List.map fromInt |> String.join ","


{-| Logs an error if box does not exist.
TODO: replace Boxes parameter by Model?
-}
mapByIdOrLog : BoxId -> Boxes -> Maybe Box
mapByIdOrLog boxId boxes =
  case mapById boxId boxes of
    Just box -> Just box
    Nothing -> illegalMapId "mapByIdOrLog" boxId Nothing


{-| TODO: replace Boxes parameter by Model? -}
mapById : BoxId -> Boxes -> Maybe Box
mapById boxId boxes =
  boxes |> Dict.get boxId


{-| TODO: replace Boxes parameter by Model? -}
hasMap : BoxId -> Boxes -> Bool
hasMap boxId boxes =
  boxes |> Dict.member boxId


addMap : BoxId -> Model -> Model
addMap boxId model =
  { model | boxes = model.boxes |> Dict.insert
    boxId
    (Box boxId (Rectangle 0 0 0 0) Dict.empty)
  }


updateMapRect : BoxId -> (Rectangle -> Rectangle) -> Model -> Model
updateMapRect boxId rectFunc model =
  { model | boxes = updateMaps
    boxId
    (\box ->
      { box | rect = rectFunc box.rect }
    )
    model.boxes
  }


{-| Logs an error if box does not exist or item is not in box or is not a topic.
TODO: replace Boxes parameter by Model?
-}
topicPos : Id -> BoxId -> Boxes -> Maybe Point
topicPos topicId boxId boxes =
  case topicProps topicId boxId boxes of
    Just { pos } -> Just pos
    Nothing -> fail "topicPos" {topicId = topicId, boxId = boxId} Nothing


{-| Logs an error if box does not exist or if topic is not in box -}
setTopicPos : Id -> BoxId -> Point -> Model -> Model
setTopicPos topicId boxId pos model =
  model |> updateTopicProps topicId boxId
    (\props -> { props | pos = pos })


{-| Logs an error if box does not exist or if topic is not in box -}
setTopicPosByDelta : Id -> BoxId -> Delta -> Model -> Model
setTopicPosByDelta topicId boxId delta model =
  model |> updateTopicProps topicId boxId
    (\props ->
      { props | pos =
        Point
          (props.pos.x + delta.x)
          (props.pos.y + delta.y)
      }
    )


{-| TODO: replace Boxes parameter by Model? -}
topicSize : Id -> BoxId -> Boxes -> Maybe Size
topicSize topicId boxId boxes =
  case topicProps topicId boxId boxes of
    Just { size } -> Just size
    Nothing -> fail "topicSize" {topicId = topicId, boxId = boxId} Nothing


{-| Logs an error if box does not exist or if topic is not in box -}
setTopicSize : Id -> BoxId -> Size -> Model -> Model
setTopicSize topicId boxId size model =
  model |> updateTopicProps topicId boxId
    (\props -> { props | size = size })


{-| TODO: replace Boxes parameter by Model? -}
displayMode : Id -> BoxId -> Boxes -> Maybe DisplayMode
displayMode topicId boxId boxes =
  case topicProps topicId boxId boxes of
    Just props -> Just props.displayMode
    Nothing -> fail "displayMode" {topicId = topicId, boxId = boxId} Nothing


{-| Logs an error if box does not exist or if topic is not in box -}
setDisplayMode : Id -> BoxId -> DisplayMode -> Model -> Model
setDisplayMode topicId boxId display model =
  model |> updateTopicProps topicId boxId
    (\props -> { props | displayMode = display })


{-| TODO: replace Boxes parameter by Model? -}
topicProps : Id -> BoxId -> Boxes -> Maybe TopicProps
topicProps topicId boxId boxes =
  case mapItemById topicId boxId boxes of
    Just mapItem ->
      case mapItem.props of
        TopicV props -> Just props
        AssocV _ -> topicMismatch "topicProps" topicId Nothing
    Nothing -> fail "topicProps" {topicId = topicId, boxId = boxId} Nothing


{-| Logs an error if box does not exist or if topic is not in box -}
updateTopicProps : Id -> BoxId -> (TopicProps -> TopicProps) -> Model -> Model
updateTopicProps topicId boxId propsFunc model =
  { model | boxes = model.boxes |> updateMaps boxId
    (\box ->
      { box | items = box.items |> Dict.update topicId
        (\mapItem_ ->
          case mapItem_ of
            Just mapItem ->
              case mapItem.props of
                TopicV props -> Just
                  { mapItem | props = TopicV (propsFunc props) }
                AssocV _ -> topicMismatch "updateTopicProps" topicId Nothing
            Nothing -> illegalItemId "updateTopicProps" topicId Nothing
        )
      }
    )
  }


{-| Initial props for a newly revealed topic -}
initItemProps : Id -> Model -> ViewProps
initItemProps itemId model =
  case itemById itemId model of
    Just item ->
      case item.info of
        Topic _ -> TopicV <| initTopicProps itemId model
        Assoc _ -> AssocV {}
    Nothing -> AssocV {} -- error is already logged


{-| Initial props for a newly revealed topic -}
initTopicProps : Id -> Model -> TopicProps
initTopicProps topicId model =
  TopicProps
    ( Point 0 0 ) -- TODO, see also MapRenderer's viewLimboAssoc()
    C.topicSize
    ( if hasMap topicId model.boxes then
        BoxD BlackBox
      else
        TopicD LabelOnly
    )


{-| Logs an error if box does not exist or item is not in box.
TODO: replace Boxes parameter by Model?
-}
mapItemById : Id -> BoxId -> Boxes -> Maybe BoxItem
mapItemById itemId boxId boxes =
  mapByIdOrLog boxId boxes |> Maybe.andThen
    (\box ->
      case box.items |> Dict.get itemId of
        Just mapItem -> Just mapItem
        Nothing -> itemNotInMap "mapItemById" itemId box.id Nothing
    )


isItemInMapDeep : Id -> BoxId -> Model -> Bool
isItemInMapDeep itemId boxId model =
  if itemId == boxId then
    True
  else
    case mapById boxId model.boxes of
      Just box -> box.items |> Dict.keys |> List.any
        (\id -> isItemInMapDeep itemId id model)
      Nothing -> False


{-| Logs an error if box does not exist. -}
isItemInMap : Id -> BoxId -> Model -> Bool
isItemInMap itemId boxId model =
  case mapByIdOrLog boxId model.boxes of
    Just box -> box.items |> Dict.member itemId
    Nothing -> False


{-| Adds an item to a box and creates a connecting association.
Presumption: the item is not yet contained in the box. Otherwise the existing box-item would be
overridden and another association still be created. This is not what you want.
Can be used for both, topics and associations.
-}
putItemOnMap : Id -> ViewProps -> BoxId -> Model -> Model
putItemOnMap itemId props boxId model =
  let
    (newModel, parentAssocId) = addAssoc
      "dmx.composition"
      "dmx.child" itemId
      "dmx.parent" boxId
      model
    mapItem = BoxItem itemId parentAssocId False False props -- hidden=False, pinned=False
    _ = info "putItemOnMap"
      { itemId = itemId, parentAssocId = parentAssocId, props = props, boxId = boxId}
  in
  { newModel | boxes = newModel.boxes |> updateMaps
      boxId
      (\box -> { box | items = box.items |> Dict.insert itemId mapItem })
  }


{-| Presumption: the item *is* contained in the box. Sets its "hidden" flag to False.
Can be used for both, topics and associations.
If the item is *not* contained in the box, or its "hidden" flag is False already, its a no-op.
Logs an error if box does not exist.
-}
showItem : Id -> BoxId -> Model -> Model
showItem itemId boxId model =
  { model | boxes = model.boxes |> updateMaps
    boxId
    (\box ->
      { box | items = box.items |> Dict.update itemId
        (\maybeItem ->
          case maybeItem of
            Just mapItem -> Just { mapItem | hidden = False }
            Nothing -> Nothing
        )
      }
    )
  }


hideItem : Id -> BoxId -> Model -> Model
hideItem itemId boxId model =
  { model | boxes = model.boxes |> updateMaps
    boxId
    (\box -> { box | items = hideItem_ itemId box.items model })
  }


hideItem_ : Id -> BoxItems -> Model -> BoxItems
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


{-| Logs an error if box does not exist.
TODO: replace Boxes parameter by Model?
-}
updateMaps : BoxId -> (Box -> Box) -> Boxes -> Boxes
updateMaps boxId mapFunc boxes =
  boxes |> Dict.update boxId
    (\map_ ->
      case map_ of
        Just box -> Just (mapFunc box)
        Nothing -> illegalMapId "updateMaps" boxId Nothing
    )


mapAssocsOfPlayer_ : Id -> BoxItems -> Model -> List Id
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
isMapTopic : BoxItem -> Bool
isMapTopic item =
  case item.props of
    TopicV _ -> True
    AssocV _ -> False


{-| useful as a filter predicate
-}
isMapAssoc : BoxItem -> Bool
isMapAssoc item =
  not (isMapTopic item)


{-| useful as a filter predicate
-}
isVisible : BoxItem -> Bool
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
isSelected itemId boxId model =
  model.selection |> List.any
    (\(id, boxPath) ->
      case boxPath of
        mapId_ :: _ -> itemId == id && boxId == mapId_
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
        boxId :: _ -> Just boxId
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
itemNotInMap funcName itemId boxId val =
  logError funcName ("item " ++ fromInt itemId ++ " not in box " ++ fromInt boxId) val


topicMismatch : String -> Id -> a -> a
topicMismatch funcName id val =
  logError funcName (fromInt id ++ " is not a Topic but an Assoc") val


assocMismatch : String -> Id -> a -> a
assocMismatch funcName id val =
  logError funcName (fromInt id ++ " is not an Assoc but a Topic") val


illegalMapId : String -> Id -> a -> a
illegalMapId funcName id val =
  illegalId funcName "Box" id val


illegalItemId : String -> Id -> a -> a
illegalItemId funcName id val =
  illegalId funcName "Item" id val


illegalId : String -> String -> Id -> a -> a
illegalId funcName item id val =
  logError funcName (fromInt id ++ " is an illegal " ++ item ++ " ID") val
