module Render.TopicMap.API exposing (fullscreen, isFullscreen, byId, byIdOrLog, update,
  updateRect, updateScrollPos, visibleTopics, topicPos, setTopicPos, updateTopicPos,
  displayMode, setDisplayMode, updateDisplayMode, topicProps, initTopicProps, initTopicPos,
  assocGeometry, hasItem, hasDeepItem, addBox, addItem, revealItem, removeItem, removeItem_,
  revelationBoxId, revelationBoxPath, landingTarget, landingBoxPath, isEmpty, isUnboxed,
  isTopic, isAssoc, isVisible, isPinned, mapTitle, elemId, firstId, fromPath)

import Config as C
import Feature.Search exposing (SearchResult(..))
import Feature.SelAPI as SelAPI
import Item
import Model exposing (Model)
import ModelParts exposing (Id, ItemInfo(..), AssocInfo, AssocType(..), Point, Rectangle, BoxId,
  BoxPath)
import Render.TopicMap exposing (TopicMap, MapItems, MapItem, Visibility(..), Pinned(..),
  ItemProps(..), TopicProps, DisplayMode(..), TopicDisplay(..), BoxDisplay(..))
import Utils as U

import Dict
import String exposing (fromInt)



fullscreen : Model -> Maybe TopicMap
fullscreen model =
  byIdOrLog model.boxId model


isFullscreen : BoxId -> Model -> Bool
isFullscreen boxId model =
  boxId == model.boxId


{-| Logs an error if box does not exist. -}
byIdOrLog : BoxId -> Model -> Maybe TopicMap
byIdOrLog boxId model =
  case byId boxId model of
    Just box -> Just box
    Nothing -> U.illegalBoxId "byIdOrLog" boxId Nothing


byId : BoxId -> Model -> Maybe TopicMap
byId boxId model =
  model.topicMap |> Dict.get boxId


-- TODO: only init renderer-specific data: (Rectangle 0 0 0 0) (Point 0 0) Dict.empty
addBox : BoxId -> Model -> Model
addBox boxId model =
  let
    box = TopicMap boxId (Rectangle 0 0 0 0) (Point 0 0) Dict.empty
  in
  model |> addBox_ box


addBox_ : TopicMap -> Model -> Model
addBox_ box ({topicMap} as model) =
  { model | topicMap = topicMap |> Dict.insert box.id box }


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


visibleTopics : TopicMap -> List MapItem
visibleTopics box =
  box.items |> Dict.values |> List.filter isTopic |> List.filter isVisible


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


{-| Logs an error if box does not exist, or if topic is not in box -}
updateTopicPos : Id -> BoxId -> (Point -> Point) -> Model -> Model
updateTopicPos topicId boxId transform model =
  model
    |> updateTopicProps_ topicId boxId
      (\props -> { props | pos = transform props.pos })


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


assocGeometry : AssocInfo -> BoxId -> Model -> Maybe (Point, Point)
assocGeometry assoc boxId model =
  let
    pos1 = topicPos assoc.player1 boxId model
    pos2 = topicPos assoc.player2 boxId model
  in
  case Maybe.map2 (\p1 p2 -> (p1, p2)) pos1 pos2 of
    Just geometry -> Just geometry
    Nothing -> U.fail "assocGeometry" { assoc = assoc, boxId = boxId } Nothing


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
itemByIdOrLog_ : Id -> BoxId -> Model -> Maybe MapItem
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
        (\id -> hasDeepItem id itemId model) -- recursion
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
    boxItem = MapItem itemId boxAssocId (Visible Unpinned) props
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


{-| Removes an item from a box, along its associations in that box context.
No-op if the item is *not* contained in the box.
Convenience API to operate on given MapItems directly
-}
removeItem_ : Id -> MapItems -> Model -> MapItems
removeItem_ itemId items model =
  assocsOfPlayer_ itemId items model |> List.foldr
    (\assocId itemsAcc -> removeItem_ assocId itemsAcc model) -- recursion
    (removeItem__ itemId items)


{-| Removes an item (sets its visibility to Removed) from a set of MapItems.
No-op if the item is *not* contained in the set of MapItems.
Low-level API that does NOT remove the item's associations.
-}
removeItem__ : Id -> MapItems -> MapItems
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
update : BoxId -> (TopicMap -> TopicMap) -> Model -> Model
update boxId transform ({topicMap} as model) =
  { model | topicMap =
    topicMap |> Dict.update boxId
      (\maybeBox ->
        case maybeBox of
          Just box -> Just (transform box)
          Nothing -> U.illegalBoxId "update" boxId Nothing
      )
  }


{-| Returns a player's associations (their Ids) in the given box context (MapItems).
Low-level API to operate on given MapItems directly.
-}
assocsOfPlayer_ : Id -> MapItems -> Model -> List Id
assocsOfPlayer_ playerId items model =
  items
    |> Dict.values
    |> List.filter isAssoc
    |> List.map .id
    |> List.filter (Item.hasPlayer playerId model)


--

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


{- The box where to reveal search/traversal results -}
revelationBoxId : Model -> Maybe BoxId
revelationBoxId model =
  case revelationBoxPath model of
    Just (boxId :: _) -> Just boxId
    _ -> Nothing


{- The box where to reveal search/traversal results, entire path -}
revelationBoxPath : Model -> Maybe BoxPath
revelationBoxPath model =
  case model.search.result of
    Topics _ _ ->
      Just <| landingBoxPath model
    RelTopics _ _ ->
      case SelAPI.single model of
        Just (_, boxPath) -> Just boxPath
        Nothing -> Nothing
    NoSearch -> Nothing


{- The landing box as a selection target. For a fullscreen box Nothing is returned. -}
landingTarget : Model -> Maybe (BoxId, BoxPath)
landingTarget model =
  case landingBoxPath model of
    [] -> Nothing
    [ boxId ] -> Nothing -- The fullscreen box is never selected
    boxId :: boxPath -> Just (boxId, boxPath)


{- The box where created things and search results land, entire box path, never empty.
Can be the fullsreen box or a nested box. -}
landingBoxPath : Model -> BoxPath
landingBoxPath model =
  case SelAPI.single model of
    Just (id, boxPath) ->
      let
        boxId = firstId boxPath
      in
      case displayMode id boxId model of
        Just (BoxD WhiteBox) -> id :: boxPath
        _ -> [ model.boxId ]
    Nothing -> [ model.boxId ]


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
isTopic : MapItem -> Bool
isTopic item =
  case item.props of
    TopicP _ -> True
    AssocP _ -> False


{-| useful as a filter predicate -}
isAssoc : MapItem -> Bool
isAssoc item =
  not (isTopic item)


{-| useful as a filter predicate -}
isVisible : MapItem -> Bool
isVisible item =
  case item.visibility of
    Visible _ -> True
    Removed -> False


isPinned : MapItem -> Bool
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
