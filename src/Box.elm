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
  model |> activeBox |> isRootBox


isRootBox : Id -> Bool
isRootBox id =
  id == 0


isActiveBox : BoxId -> Model -> Bool
isActiveBox boxId model =
  activeBox model == boxId


activeBox : Model -> BoxId
activeBox model =
  firstId model.boxPath


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
boxByIdOrLog : BoxId -> Boxes -> Maybe Box
boxByIdOrLog boxId boxes =
  case boxById boxId boxes of
    Just box -> Just box
    Nothing -> U.illegalBoxId "boxByIdOrLog" boxId Nothing


{-| TODO: replace Boxes parameter by Model? -}
boxById : BoxId -> Boxes -> Maybe Box
boxById boxId boxes =
  boxes |> Dict.get boxId


{-| TODO: replace Boxes parameter by Model? -}
isBox : Id -> Boxes -> Bool
isBox id boxes =
  boxes |> Dict.member id


addBox : BoxId -> Model -> Model
addBox boxId model =
  { model | boxes = model.boxes |> Dict.insert
    boxId
    (Box boxId (Rectangle 0 0 0 0) Dict.empty)
  }


updateBoxRect : BoxId -> (Rectangle -> Rectangle) -> Model -> Model
updateBoxRect boxId transform model =
  { model | boxes = updateBoxes
    boxId
    (\box ->
      { box | rect = transform box.rect }
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
    Nothing -> U.fail "topicPos" {topicId = topicId, boxId = boxId} Nothing


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
    Nothing -> U.fail "topicSize" {topicId = topicId, boxId = boxId} Nothing


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
    Nothing -> U.fail "displayMode" {topicId = topicId, boxId = boxId} Nothing


{-| Logs an error if box does not exist or if topic is not in box -}
setDisplayMode : Id -> BoxId -> DisplayMode -> Model -> Model
setDisplayMode topicId boxId display model =
  model |> updateTopicProps topicId boxId
    (\props -> { props | displayMode = display })


{-| TODO: replace Boxes parameter by Model? -}
topicProps : Id -> BoxId -> Boxes -> Maybe TopicProps
topicProps topicId boxId boxes =
  case boxItemById topicId boxId boxes of
    Just boxItem ->
      case boxItem.props of
        TopicV props -> Just props
        AssocV _ -> U.topicMismatch "topicProps" topicId Nothing
    Nothing -> U.fail "topicProps" {topicId = topicId, boxId = boxId} Nothing


{-| Logs an error if box does not exist or if topic is not in box -}
updateTopicProps : Id -> BoxId -> (TopicProps -> TopicProps) -> Model -> Model
updateTopicProps topicId boxId transform model =
  { model | boxes = model.boxes |> updateBoxes boxId
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
  case Item.itemById itemId model of
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
    (case isBox topicId model.boxes of
      True -> BoxD BlackBox
      False -> TopicD LabelOnly
    )


initPos : BoxId -> Point
initPos boxId =
  case isRootBox boxId of
    True -> C.nestedBoxOffset
    False -> Point 0 0


{-| Logs an error if box does not exist or item is not in box.
TODO: replace Boxes parameter by Model?
-}
boxItemById : Id -> BoxId -> Boxes -> Maybe BoxItem
boxItemById itemId boxId boxes =
  boxByIdOrLog boxId boxes |> Maybe.andThen
    (\box ->
      case box.items |> Dict.get itemId of
        Just boxItem -> Just boxItem
        Nothing -> U.itemNotInBox "boxItemById" itemId box.id Nothing
    )


boxHasDeepItem : BoxId -> Id -> Model -> Bool
boxHasDeepItem boxId itemId model =
  if itemId == boxId then
    True
  else
    case boxById boxId model.boxes of
      Just box -> box.items |> Dict.keys |> List.any
        (\id -> boxHasDeepItem id itemId model)
      Nothing -> False


{-| Logs an error if box does not exist. -}
boxHasItem : BoxId -> Id -> Model -> Bool
boxHasItem boxId itemId model =
  case boxByIdOrLog boxId model.boxes of
    Just box -> box.items |> Dict.member itemId
    Nothing -> False


{-| Adds an item to a box and creates a connecting association.
Presumption: the item is not yet contained in the box. Otherwise the existing box-item would be
overridden and another association still be created. This is not what you want.
Can be used for both, topics and associations.
-}
addItemToBox : Id -> ViewProps -> BoxId -> Model -> Model
addItemToBox itemId props boxId model =
  let
    (newModel, parentAssocId) = Item.addAssoc
      "dmx.composition"
      "dmx.child" itemId
      "dmx.parent" boxId
      model
    boxItem = BoxItem itemId parentAssocId False False props -- hidden=False, pinned=False
    _ = U.info "addItemToBox"
      { itemId = itemId, parentAssocId = parentAssocId, props = props, boxId = boxId}
  in
  { newModel | boxes = newModel.boxes |> updateBoxes
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
  { model | boxes = model.boxes |> updateBoxes
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
  { model | boxes = model.boxes |> updateBoxes
    boxId
    (\box -> { box | items = hideItem_ itemId box.items model })
  }


hideItem_ : Id -> BoxItems -> Model -> BoxItems
hideItem_ itemId items model =
  boxAssocsOfPlayer_ itemId items model |> List.foldr
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
updateBoxes : BoxId -> (Box -> Box) -> Boxes -> Boxes
updateBoxes boxId transform boxes =
  boxes |> Dict.update boxId
    (\box_ ->
      case box_ of
        Just box -> Just (transform box)
        Nothing -> U.illegalBoxId "updateBoxes" boxId Nothing
    )


boxAssocsOfPlayer_ : Id -> BoxItems -> Model -> List Id
boxAssocsOfPlayer_ playerId items model =
  items
  |> Dict.values
  |> List.filter isBoxAssoc
  |> List.map .id
  |> List.filter (hasPlayer playerId model)


hasPlayer : Id -> Model -> Id -> Bool
hasPlayer playerId model assocId =
  case Item.assocById assocId model of
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
isBoxTopic : BoxItem -> Bool
isBoxTopic item =
  case item.props of
    TopicV _ -> True
    AssocV _ -> False


{-| useful as a filter predicate
-}
isBoxAssoc : BoxItem -> Bool
isBoxAssoc item =
  not (isBoxTopic item)


{-| useful as a filter predicate
-}
isVisible : BoxItem -> Bool
isVisible item =
  not item.hidden
