module Box exposing (..)

import Config as C
import Item
import Model exposing (Model)
import ModelHelper exposing (..)
import Utils as U

import Dict
import String exposing (fromInt)



isAtRoot : Model -> Bool
isAtRoot model =
  model |> active |> isRoot


isRoot : Id -> Bool
isRoot id =
  id == 0


isActive : BoxId -> Model -> Bool -- TODO: rename "isFullscreen"?
isActive boxId model =
  active model == boxId


active : Model -> BoxId
active model =
  model.boxId


activeName : Model -> String
activeName model =
  case Item.topicById (active model) model of
    Just topic -> Item.topicLabel topic
    Nothing -> "??"


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


{-| Logs an error if box does not exist.
TODO: replace Boxes parameter by Model?
-}
byIdOrLog : BoxId -> Boxes -> Maybe Box
byIdOrLog boxId boxes =
  case byId boxId boxes of
    Just box -> Just box
    Nothing -> U.illegalBoxId "byIdOrLog" boxId Nothing


{-| TODO: replace Boxes parameter by Model? -}
byId : BoxId -> Boxes -> Maybe Box
byId boxId boxes =
  boxes |> Dict.get boxId


{-| Presumption: the item exists already.
TODO: create item along with box
-}
addBox : BoxId -> Model -> Model
addBox boxId model =
  { model | boxes = model.boxes |> Dict.insert
    boxId
    (Box boxId (Rectangle 0 0 0 0) Dict.empty)
  }


updateRect : BoxId -> (Rectangle -> Rectangle) -> Model -> Model
updateRect boxId transform model =
  { model | boxes = update
    boxId
    (\box ->
      { box | rect = transform box.rect }
    )
    model.boxes
  }


{-| Logs an error if box does not exist, or topic is not in box, or ID refers not a topic (but
an association).
-}
topicPos : Id -> BoxId -> Model -> Maybe Point
topicPos topicId boxId model =
  case topicProps topicId boxId model of
    Just { pos } -> Just pos
    Nothing -> U.fail "topicPos" {topicId = topicId, boxId = boxId} Nothing


{-| Logs an error if box does not exist or if topic is not in box -}
setTopicPos : Id -> BoxId -> Point -> Model -> Model
setTopicPos topicId boxId pos model =
  model |> updateTopicProps topicId boxId
    (\props -> { props | pos = pos })


{-| Logs an error if box does not exist or if topic is not in box
TODO: make it "updateTopicPos" and take a transform function
-}
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


{-| Logs an error if box does not exist, or topic is not in box, or ID refers not a topic (but
an association).
-}
topicSize : Id -> BoxId -> Model -> Maybe Size
topicSize topicId boxId model =
  case topicProps topicId boxId model of
    Just { size } -> Just size
    Nothing -> U.fail "topicSize" {topicId = topicId, boxId = boxId} Nothing


{-| Logs an error if box does not exist, or topic is not in box -}
setTopicSize : Id -> BoxId -> Size -> Model -> Model
setTopicSize topicId boxId size model =
  model |> updateTopicProps topicId boxId
    (\props -> { props | size = size })


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
  model |> updateTopicProps topicId boxId
    (\props -> { props | displayMode = transform props.displayMode })


{-| Logs an error if box does not exist, or topic is not in box, or ID refers not a topic (but
an association).
-}
topicProps : Id -> BoxId -> Model -> Maybe TopicProps
topicProps topicId boxId model =
  case itemByIdOrLog topicId boxId model.boxes of
    Just boxItem ->
      case boxItem.props of
        TopicV props -> Just props
        AssocV _ -> U.topicMismatch "topicProps" topicId Nothing
    Nothing -> U.fail "topicProps" {topicId = topicId, boxId = boxId} Nothing


{-| Logs an error if box does not exist or if topic is not in box -}
updateTopicProps : Id -> BoxId -> (TopicProps -> TopicProps) -> Model -> Model
updateTopicProps topicId boxId transform model =
  { model | boxes = model.boxes |> update boxId
    (\box ->
      { box | items = box.items |> Dict.update topicId
        (\boxItem_ ->
          case boxItem_ of
            Just boxItem ->
              case boxItem.props of
                TopicV props -> Just
                  { boxItem | props = TopicV (transform props) }
                AssocV _ -> U.topicMismatch "updateTopicProps" topicId Nothing
            Nothing -> U.illegalItemId "updateTopicProps" topicId Nothing
        )
      }
    )
  }


{-| Initial props for a newly revealed item -}
initItemProps : Id -> BoxId -> Model -> ViewProps
initItemProps itemId boxId model =
  case Item.byId itemId model of
    Just item ->
      case item.info of
        Topic _ -> TopicV <| initTopicProps itemId boxId model
        Assoc _ -> AssocV {}
    Nothing -> AssocV {} -- error is already logged


{-| Initial props for a newly revealed topic -}
initTopicProps : Id -> BoxId -> Model -> TopicProps
initTopicProps topicId boxId model =
  TopicProps
    (initPos boxId)
    C.topicSize
    (case Item.isBox topicId model of
      True -> BoxD BlackBox
      False -> TopicD LabelOnly
    )


initPos : BoxId -> Point
initPos boxId =
  case isRoot boxId of
    True -> C.nestedBoxOffset
    False -> Point 0 0


{-| Logs an error if box does not exist or item is not in box.
TODO: replace Boxes parameter by Model?
-}
itemByIdOrLog : Id -> BoxId -> Boxes -> Maybe BoxItem
itemByIdOrLog itemId boxId boxes =
  byIdOrLog boxId boxes |> Maybe.andThen
    (\box ->
      case box.items |> Dict.get itemId of
        Just boxItem -> Just boxItem
        Nothing -> U.itemNotInBox "itemByIdOrLog" itemId box.id Nothing
    )


{-| Logs an error if box does not exist. -}
hasItem : BoxId -> Id -> Model -> Bool
hasItem boxId itemId model =
  case byIdOrLog boxId model.boxes of
    Just box -> box.items |> Dict.member itemId
    Nothing -> False


hasDeepItem : BoxId -> Id -> Model -> Bool
hasDeepItem boxId itemId model =
  if itemId == boxId then
    True
  else
    case byId boxId model.boxes of
      Just box -> box.items |> Dict.keys |> List.any
        (\id -> hasDeepItem id itemId model)
      Nothing -> False


{-| Adds an item to a box and creates a connecting association.
Presumption: the item is not yet contained in the box. Otherwise the existing box-item would be
overridden and another association still be created. This is not what you want.
Can be used for both, topics and associations.
-}
addItem : Id -> ViewProps -> BoxId -> Model -> Model
addItem itemId props boxId model =
  let
    (newModel, parentAssocId) = Item.addAssoc
      "dmx.composition"
      "dmx.child" itemId
      "dmx.parent" boxId
      model
    boxItem = BoxItem itemId parentAssocId False False props -- hidden=False, pinned=False
    _ = U.info "addItem"
      { itemId = itemId, parentAssocId = parentAssocId, props = props, boxId = boxId}
  in
  { newModel | boxes = newModel.boxes |> update
      boxId
      (\box -> { box | items = box.items |> Dict.insert itemId boxItem })
  }


{-| Presumption: the item *is* contained in the box. Sets its "hidden" flag to False.
Can be used for both, topics and associations.
If the item is *not* contained in the box, or its "hidden" flag is False already, its a no-op.
Logs an error if box does not exist.
-}
showItem : Id -> BoxId -> Model -> Model
showItem itemId boxId model =
  { model | boxes = model.boxes |> update
    boxId
    (\box ->
      { box | items = box.items |> Dict.update itemId
        (\maybeItem ->
          case maybeItem of
            Just boxItem -> Just { boxItem | hidden = False }
            Nothing -> Nothing
        )
      }
    )
  }


hideItem : Id -> BoxId -> Model -> Model
hideItem itemId boxId model =
  { model | boxes = model.boxes |> update
    boxId
    (\box -> { box | items = hideItem_ itemId box.items model })
  }


hideItem_ : Id -> BoxItems -> Model -> BoxItems
hideItem_ itemId items model =
  assocsOfPlayer_ itemId items model |> List.foldr
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
update : BoxId -> (Box -> Box) -> Boxes -> Boxes
update boxId transform boxes =
  boxes |> Dict.update boxId
    (\box_ ->
      case box_ of
        Just box -> Just (transform box)
        Nothing -> U.illegalBoxId "update" boxId Nothing
    )


assocsOfPlayer_ : Id -> BoxItems -> Model -> List Id
assocsOfPlayer_ playerId items model =
  items
  |> Dict.values
  |> List.filter isAssoc
  |> List.map .id
  |> List.filter (Item.hasPlayer playerId model)


{-| useful as a filter predicate -}
isTopic : BoxItem -> Bool
isTopic item =
  case item.props of
    TopicV _ -> True
    AssocV _ -> False


{-| useful as a filter predicate -}
isAssoc : BoxItem -> Bool
isAssoc item =
  not (isTopic item)


{-| useful as a filter predicate -}
isVisible : BoxItem -> Bool
isVisible item =
  not item.hidden
