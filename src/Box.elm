module Box exposing (..)

import Config as C
import Item
import Model exposing (Model)
import ModelParts exposing (..)
import Utils as U

import Dict
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
TODO: create item along with box
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
  |> updateTopicProps topicId boxId
    (\props -> { props | displayMode = transform props.displayMode })


{-| Logs an error if box does not exist, or topic is not in box, or ID refers not a topic (but
an association).
-}
topicProps : Id -> BoxId -> Model -> Maybe TopicProps
topicProps topicId boxId model =
  case itemByIdOrLog topicId boxId model of
    Just boxItem ->
      case boxItem.props of
        TopicV props -> Just props
        AssocV _ -> U.topicMismatch "topicProps" topicId Nothing
    Nothing -> U.fail "topicProps" {topicId = topicId, boxId = boxId} Nothing


{-| Logs an error if box does not exist or if topic is not in box -}
updateTopicProps : Id -> BoxId -> (TopicProps -> TopicProps) -> Model -> Model
updateTopicProps topicId boxId transform model =
  model |> update boxId
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
    ( initTopicPos boxId model )
    ( case Item.isBox topicId model of
      True -> BoxD BlackBox
      False -> TopicD LabelOnly
    )


initTopicPos : BoxId -> Model -> Point
initTopicPos boxId model =
  case byIdOrLog boxId model of
    Just box ->
      Point
        (C.initTopicPos.x + box.rect.x1 + box.scroll.x)
        (C.initTopicPos.y + box.rect.y1 + box.scroll.y)
    Nothing -> Point 0 0 -- error is already logged


{-| Logs an error if box does not exist or item is not in box. -}
itemByIdOrLog : Id -> BoxId -> Model -> Maybe BoxItem
itemByIdOrLog itemId boxId model =
  byIdOrLog boxId model |> Maybe.andThen
    (\box ->
      case box.items |> Dict.get itemId of
        Just boxItem -> Just boxItem
        Nothing -> U.itemNotInBox "itemByIdOrLog" itemId box.id Nothing
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
Can be used for both, topics and associations.
-}
addItem : Id -> ViewProps -> BoxId -> Model -> Model
addItem itemId props boxId model =
  let
    ( newModel, parentAssocId ) = Item.addAssoc
      "dmx.composition"
      "dmx.child" itemId
      "dmx.parent" boxId
      model
    boxItem = BoxItem itemId parentAssocId False False props -- hidden=False, pinned=False
    _ = U.info "addItem"
      { itemId = itemId, parentAssocId = parentAssocId, props = props, boxId = boxId}
  in
  newModel |> update boxId
    (\box -> { box | items = box.items |> Dict.insert itemId boxItem })


{-| Presumption: the item *is* contained in the box. Sets its "hidden" flag to False.
Can be used for both, topics and associations.
If the item is *not* contained in the box, or its "hidden" flag is False already, its a no-op.
Logs an error if box does not exist.
-}
showItem : Id -> BoxId -> Model -> Model
showItem itemId boxId model =
  model |> update boxId
    (\box ->
      { box | items = box.items |> Dict.update itemId
        (\maybeItem ->
          case maybeItem of
            Just boxItem -> Just { boxItem | hidden = False }
            Nothing -> Nothing
        )
      }
    )


removeItem : Id -> BoxId -> Model -> Model
removeItem itemId boxId model =
  model |> update boxId
    (\box -> { box | items = removeItem_ itemId box.items model })


removeItem_ : Id -> BoxItems -> Model -> BoxItems
removeItem_ itemId items model =
  assocsOfPlayer_ itemId items model |> List.foldr
    (\assocId itemsAcc -> removeItem_ assocId itemsAcc model)
    (items |> Dict.update
      itemId
      (\item_ ->
        case item_ of
          Just item -> Just { item | hidden = True }
          Nothing -> Nothing
      )
    )


{-| Logs an error if box does not exist. -}
update : BoxId -> (Box -> Box) -> Model -> Model
update boxId transform model =
  { model | boxes = model.boxes |> Dict.update boxId
    (\box_ ->
      case box_ of
        Just box -> Just (transform box)
        Nothing -> U.illegalBoxId "update" boxId Nothing
    )
  }


visibleTopics : Box -> List BoxItem
visibleTopics box =
  box.items
  |> Dict.values
  |> List.filter isTopic
  |> List.filter isVisible


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


mapTitle : Model -> String
mapTitle model =
  case Item.topicById model.boxId model of
    Just topic -> Item.topicLabel topic
    Nothing -> "??"


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
