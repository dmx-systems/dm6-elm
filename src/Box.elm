module Box exposing (byId, byIdOrLog, update, updateRect, updateScrollPos, topicPos,
  setTopicPos, setTopicPosByDelta, displayMode, setDisplayMode, updateDisplayMode, topicProps,
  initTopicProps, initTopicPos, hasItem, hasDeepItem, addBox, addItem, revealItem, removeItem,
  removeItem_, deleteItem, isEmpty, isUnboxed, isTopic, isAssoc, isVisible, isPinned, mapTitle,
  elemId, firstId, fromPath)

import Config as C
import Item
import Model exposing (Model)
import ModelParts exposing (..)
import Utils as U

import Dict
import Set
import String exposing (fromInt)



{-| Logs an error if box does not exist. -}
byIdOrLog : BoxId -> Model -> Maybe Box
byIdOrLog boxId model =
  case byId boxId model of
    Just box -> Just box
    Nothing -> U.illegalBoxId "byIdOrLog" boxId Nothing


byId : BoxId -> Model -> Maybe Box
byId boxId model =
  model.boxes |> Dict.get boxId


{-| Presumption: the item exists already.
TODO: create item along with box?
-}
addBox : BoxId -> Model -> Model
addBox boxId model =
  { model | boxes = model.boxes |> Dict.insert
    boxId
    (Box boxId (Rectangle 0 0 0 0) (Point 0 0) Dict.empty)
  }


updateRect : BoxId -> (Rectangle -> Rectangle) -> Model -> Model
updateRect boxId transform model =
  model |> update boxId
    (\box ->
      { box | rect = transform box.rect }
    )


updateScrollPos : BoxId -> (Point -> Point) -> Model -> Model
updateScrollPos boxId transform model =
  model |> update boxId
    (\box ->
      { box | scroll = transform box.scroll }
    )


{-| Logs an error if box does not exist, or topic is not in box, or ID refers not a topic (but
an association).
-}
topicPos : Id -> BoxId -> Model -> Maybe Point
topicPos topicId boxId model =
  case topicProps topicId boxId model of
    Just { pos } -> Just pos
    Nothing -> U.fail "topicPos" {topicId = topicId, boxId = boxId} Nothing


{-| Logs an error if box does not exist, or if topic is not in box -}
setTopicPos : Id -> BoxId -> Point -> Model -> Model
setTopicPos topicId boxId pos model =
  model |> updateTopicProps_ topicId boxId
    (\props -> { props | pos = pos })


{-| Logs an error if box does not exist, or if topic is not in box
TODO: make it "updateTopicPos" and take a transform function
-}
setTopicPosByDelta : Id -> BoxId -> Delta -> Model -> Model
setTopicPosByDelta topicId boxId delta model =
  model |> updateTopicProps_ topicId boxId
    (\props ->
      { props | pos =
        Point
          (props.pos.x + delta.x)
          (props.pos.y + delta.y)
      }
    )


{-| Logs an error if box does not exist, or topic is not in box, or ID refers not a topic (but
an association).
-}
displayMode : Id -> BoxId -> Model -> Maybe DisplayMode
displayMode topicId boxId model =
  case topicProps topicId boxId model of
    Just props -> Just props.displayMode
    Nothing -> U.fail "displayMode" {topicId = topicId, boxId = boxId} Nothing


{-| Logs an error if box does not exist, or if topic is not in box -}
setDisplayMode : Id -> BoxId -> DisplayMode -> Model -> Model
setDisplayMode topicId boxId display model =
  model
    |> updateDisplayMode topicId boxId (\_ -> display)


updateDisplayMode : Id -> BoxId -> (DisplayMode -> DisplayMode) -> Model -> Model
updateDisplayMode topicId boxId transform model =
  model
    |> updateTopicProps_ topicId boxId
      (\props -> { props | displayMode = transform props.displayMode })


{-| Logs an error if box does not exist, or topic is not in box, or ID refers not a topic (but
an association).
-}
topicProps : Id -> BoxId -> Model -> Maybe TopicProps
topicProps topicId boxId model =
  case itemByIdOrLog_ topicId boxId model of
    Just boxItem ->
      case boxItem.props of
        TopicP props -> Just props
        AssocP _ -> U.topicMismatch "topicProps" topicId Nothing
    Nothing -> U.fail "topicProps" {topicId = topicId, boxId = boxId} Nothing


revealItem : Id -> BoxId -> Model -> Model
revealItem itemId boxId model =
  if hasItem boxId itemId model then
    let
      _ = U.info "revealItem" <| fromInt itemId ++ " is in " ++ fromInt boxId
    in
    showItem_ itemId boxId model
  else
    let
      _ = U.info "revealItem" <| fromInt itemId ++ " not in " ++ fromInt boxId
      props = initItemProps_ itemId boxId model
    in
    addItem itemId props boxId model


{-| Initial props for a newly revealed item -}
initItemProps_ : Id -> BoxId -> Model -> ItemProps
initItemProps_ itemId boxId model =
  case Item.byId itemId model of
    Just item ->
      case item.info of
        Topic _ -> TopicP <| initTopicProps itemId boxId model
        Assoc _ -> AssocP {}
    Nothing -> AssocP {} -- error is already logged


{-| Initial props for a newly revealed topic -}
initTopicProps : Id -> BoxId -> Model -> TopicProps
initTopicProps topicId boxId model =
  TopicProps
    ( initTopicPos boxId model )
    ( case Item.isBox topicId model of
      True -> BoxD BlackBox
      False -> TopicD LabelOnly
    )


{-| Logs an error if box does not exist. -}
initTopicPos : BoxId -> Model -> Point
initTopicPos boxId model =
  case byIdOrLog boxId model of
    Just box ->
      Point
        (C.initTopicPos.x + box.rect.x1 + box.scroll.x)
        (C.initTopicPos.y + box.rect.y1 + box.scroll.y)
    Nothing -> Point 0 0 -- error is already logged


{-| Logs an error if box does not exist, or item is not in box. -}
itemByIdOrLog_ : Id -> BoxId -> Model -> Maybe BoxItem
itemByIdOrLog_ itemId boxId model =
  byIdOrLog boxId model |> Maybe.andThen
    (\box ->
      case box.items |> Dict.get itemId of
        Just boxItem -> Just boxItem
        Nothing -> U.itemNotInBox "itemByIdOrLog_" itemId box.id Nothing
    )


{-| Logs an error if box does not exist. -}
hasItem : BoxId -> Id -> Model -> Bool
hasItem boxId itemId model =
  case byIdOrLog boxId model of
    Just box -> box.items |> Dict.member itemId
    Nothing -> False


hasDeepItem : BoxId -> Id -> Model -> Bool
hasDeepItem boxId itemId model =
  if itemId == boxId then
    True
  else
    case byId boxId model of
      Just box -> box.items |> Dict.keys |> List.any
        (\id -> hasDeepItem id itemId model)
      Nothing -> False


{-| Adds an item to a box and creates a connecting association.
Presumption: the item is not yet contained in the box. Otherwise the existing box-item would be
overridden and another association still be created. This is not what you want.
It's a generic operation: works for both, topics and associations.
-}
addItem : Id -> ItemProps -> BoxId -> Model -> Model
addItem itemId props boxId model =
  let
    ( newModel, boxAssocId ) = Item.addAssoc Hierarchy boxId itemId model
    boxItem = BoxItem itemId boxAssocId (Visible Unpinned) props
    _ = U.info "addItem"
      { itemId = itemId, boxAssocId = boxAssocId, props = props, boxId = boxId}
  in
  newModel |> update boxId
    (\box -> { box | items = box.items |> Dict.insert itemId boxItem })


{-| Sets the item's "visibility" field to Visible (Pinned=False).
Presumption: the item *is* contained in the box.
No-op if the item is *not* contained in the box, or its "visibility" field is Visible already.
Logs an error if box does not exist.
It's a generic operation: works for both, topics and associations.
-}
showItem_ : Id -> BoxId -> Model -> Model
showItem_ itemId boxId model =
  model |> update boxId
    (\box ->
      { box | items = box.items |> Dict.update itemId
        (\maybeItem ->
          case maybeItem of
            Just boxItem -> Just
              { boxItem | visibility =
                case boxItem.visibility of
                  Visible _ -> boxItem.visibility
                  Removed -> Visible Unpinned
              }
            Nothing -> Nothing
        )
      }
    )


{-| Removes an item from a box, along its associations in that box context.
No-op if the item is *not* contained in the box.
Logs an error if box does not exist.
It's a generic operation: works for both, topics and associations.
Note: while this functions supports associations as players in associations,
at the moment DM6 Elm makes no use of it.
-}
removeItem : Id -> BoxId -> Model -> Model
removeItem itemId boxId model =
  model
    |> update boxId
        (\box ->
          { box | items = removeItem_ itemId box.items model }
        )
    |> resetEmptyBox_ boxId


{-| Removes an item from a box, along its associations in that box context.
No-op if the item is *not* contained in the box.
Convenience API to operate on given BoxItems directly
-}
removeItem_ : Id -> BoxItems -> Model -> BoxItems
removeItem_ itemId items model =
  assocsOfPlayer_ itemId items model |> List.foldr
    (\assocId itemsAcc -> removeItem_ assocId itemsAcc model) -- recursion
    (removeItem__ itemId items)


{-| Removes an item (sets its visibility to Removed) from a set of BoxItems.
No-op if the item is *not* contained in the set of BoxItems.
Low-level API that does NOT remove the item's associations.
-}
removeItem__ : Id -> BoxItems -> BoxItems
removeItem__ itemId items =
  items |> Dict.update itemId
    (\maybeItem ->
      case maybeItem of
        Just item -> Just { item | visibility = Removed }
        Nothing -> Nothing
    )


{-| Canonical box transformation.
Logs an error if box does not exist.
-}
update : BoxId -> (Box -> Box) -> Model -> Model
update boxId transform model =
  { model | boxes = model.boxes |> Dict.update boxId
    (\maybeBox ->
      case maybeBox of
        Just box -> Just (transform box)
        Nothing -> U.illegalBoxId "update" boxId Nothing
    )
  }


{-| Returns a player's associations (their Ids) in the given box context (BoxItems).
Low-level API to operate on given BoxItems directly.
-}
assocsOfPlayer_ : Id -> BoxItems -> Model -> List Id
assocsOfPlayer_ playerId items model =
  items
    |> Dict.values
    |> List.filter isAssoc
    |> List.map .id
    |> List.filter (Item.hasPlayer playerId model)


-- Delete Item

deleteItem : Id -> Model -> Model
deleteItem itemId model =
  model
    |> deleteItem_ itemId
    |> resetAllEmptyBoxes


{-| Deletes an item, along its associations, and removes them from all boxes.
Logs an error if no such item exists.
It's a generic operation: works for both, topics and associations.
Note: while this functions supports associations as players in associations,
at the moment DM6 Elm makes no use of it.
-}
deleteItem_ : Id -> Model -> Model
deleteItem_ itemId model =
  Item.assocIds itemId model
    |> Set.foldr deleteItem_ model -- recursion
    |> deleteAssocRefs_ itemId
    |> deleteItem__ itemId


{-| Removes the association ID from both player's set of association IDs.
No-op if the given ID refers not to an association (but a topic).
Logs an error no item for the given ID exists.
Low-level API for maintaining the sets of association IDs.
-}
deleteAssocRefs_ : Id -> Model -> Model
deleteAssocRefs_ assocId model =
  case Item.byId assocId model of
    Just {info} ->
      case info of
        Assoc assoc -> -- Note: assocId and assoc.id are the same
          model
            |> deleteAssocId_ assoc.id assoc.player1
            |> deleteAssocId_ assoc.id assoc.player2
        Topic _ -> model
    Nothing -> model -- error is already logged


{-| Removes an association ID from the item's set of association IDs.
No-op if the given association ID is not in the set.
Logs an error if item does not exist.
Low-level API for maintaining the association ID set.
-}
deleteAssocId_ : Id -> Id -> Model -> Model
deleteAssocId_ assocId itemId model =
  model |> Item.update itemId
    (\item ->
      {item | assocIds = item.assocIds |> Set.remove assocId}
    )


{-| Deletes an item, and removes it from all boxes.
No-op if there is no such item.
Low-level API that does NOT delete the item's associations.
-}
deleteItem__ : Id -> Model -> Model
deleteItem__ itemId model =
  { model
    | items = model.items |> Dict.remove itemId -- delete item
    , boxes = model.boxes |> Dict.map -- delete item from all boxes
        (\_ box ->
          { box | items = box.items |> Dict.remove itemId }
        )
  }


resetAllEmptyBoxes : Model -> Model
resetAllEmptyBoxes model =
  model.boxes
    |> Dict.keys
    |> List.foldr resetEmptyBox_ model


--

resetEmptyBox_ : BoxId -> Model -> Model
resetEmptyBox_ boxId model =
  case isEmpty boxId model of
    True -> model
      |> updateTopicPropsInAllBoxes_ boxId
        (\props -> { props | displayMode = BoxD BlackBox })
    False -> model


{-| Canonical TopicProps transformation.
Logs an error if box does not exist, or topic is not in box, or ID refers not a topic (but
an association).
-}
updateTopicProps_ : Id -> BoxId -> (TopicProps -> TopicProps) -> Model -> Model
updateTopicProps_ topicId boxId transform model =
  model |> update boxId
    (\box ->
      { box | items = box.items |> Dict.update topicId
        (\item_ ->
          case item_ of
            Just item ->
              case item.props of
                TopicP props -> Just
                  { item | props = TopicP (transform props) }
                AssocP _ -> U.topicMismatch "updateTopicProps_" topicId Nothing
            Nothing -> U.illegalItemId "updateTopicProps_" topicId Nothing
        )
      }
    )


-- TODO: factor out updateTopicProps_ common core
updateTopicPropsInAllBoxes_ : Id -> (TopicProps -> TopicProps) -> Model -> Model
updateTopicPropsInAllBoxes_ topicId transform model =
  { model | boxes = model.boxes |> Dict.map
    (\_ box ->
      { box | items = box.items |> Dict.update topicId
        (\item_ ->
          case item_ of
            Just item ->
              case item.props of
                TopicP props -> Just
                  { item | props = TopicP (transform props) }
                AssocP _ -> U.topicMismatch "updateTopicPropsInAllBoxes_" topicId item_
            Nothing -> Nothing
        )
      }
    )
  }


{-| Logs an error if box does not exist. -}
isEmpty : BoxId -> Model -> Bool
isEmpty boxId model =
  case byIdOrLog boxId model of
    Just box -> box.items |> Dict.values |> List.any isVisible |> not
    Nothing -> False


{-| Logs an error if box does not exist, or topic is not in box, or ID refers not a topic (but
an association).
-}
isUnboxed : Id -> BoxId -> Model -> Bool
isUnboxed topicId boxId model =
  displayMode topicId boxId model == Just (BoxD Unboxed)


{-| useful as a filter predicate -}
isTopic : BoxItem -> Bool
isTopic item =
  case item.props of
    TopicP _ -> True
    AssocP _ -> False


{-| useful as a filter predicate -}
isAssoc : BoxItem -> Bool
isAssoc item =
  not (isTopic item)


{-| useful as a filter predicate -}
isVisible : BoxItem -> Bool
isVisible item =
  case item.visibility of
    Visible _ -> True
    Removed -> False


isPinned : BoxItem -> Bool
isPinned item =
  item.visibility == Visible Pinned


mapTitle : Model -> String
mapTitle model =
  case Item.topicById model.boxId model of
    Just topic -> Item.topicLabel topic
    Nothing -> U.fail "mapTitle" model.boxId "??"


elemId : String -> Id -> BoxPath -> String
elemId name id boxPath =
  name ++ "-" ++ fromInt id ++ "," ++ fromPath boxPath


{-| Logs an error (and returns -1) if boxPath is empty.
-}
firstId : BoxPath -> BoxId
firstId boxPath =
  case boxPath of
    boxId :: _ -> boxId
    [] -> U.logError "firstId" "boxPath is empty!" -1


fromPath : BoxPath -> String
fromPath boxPath =
  boxPath |> List.map fromInt |> String.join ","
