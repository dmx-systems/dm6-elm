module TopicMap.Transfer exposing (boxContent, unboxContent)

import Item
import Model exposing (Model)
import ModelBase exposing (..)
import TopicMap.TopicMap as TM
import TopicMap.TopicMapDef exposing (TopicMap, MapItems, MapItem, Visibility(..), Pinned(..),
  ItemProps(..), AssocProps)
import Utils as U

import Dict



-- MODEL


type alias Transfer = TopicMap -> MapItems -> Model -> MapItems



-- UPDATE


{-| Removes box content from its parent box.
(Any target box can be given but de-facto it's the box's parent box.)
Presumtion: content is currently unboxed
-}
boxContent : BoxId -> BoxId -> Model -> Model
boxContent mapId targetMapId model =
  transferContent mapId targetMapId boxItems_ model


{-| Reveals box content on its parent box.
(Any target box can be given but de-facto it's the box's parent box.)
Presumtion: content is currently boxed (blackbox or whitebox)
-}
unboxContent : BoxId -> BoxId -> Model -> Model
unboxContent mapId targetMapId model =
  transferContent mapId targetMapId unboxItems_ model


transferContent : BoxId -> BoxId -> Transfer -> Model -> Model
transferContent mapId targetMapId transfer model =
  case TM.byIdOrLog mapId model of
    Just topicMap ->
      model |> TM.update targetMapId
        (\targetBox ->
          { targetBox | items = transfer topicMap targetBox.items model }
        )
    Nothing -> model


{-| Transfer function, Boxing.
Removes the content items (recursively) from the target items.
Returns the updated target items.
-}
boxItems_ : TopicMap -> MapItems -> Model -> MapItems
boxItems_ topicMap targetItems model =
  topicMap.items |> Dict.values |> List.filter TM.isVisible |> List.foldr
    (\mapItem targetItemsAcc ->
      case targetItemsAcc |> Dict.get mapItem.id of
        Just item ->
          if item |> TM.isPinned then
            -- don't box pinned items, only remove the assoc
            TM.removeItem_ -1 targetItemsAcc model -- ### FIXME: -1 "mapItem.boxAssocId"
          else
            let
              items = TM.removeItem_ mapItem.id targetItemsAcc model
            in
            case TM.byId mapItem.id model of
              Just topicMap_ -> boxItems_ topicMap_ items model -- recursion
              Nothing -> items
        Nothing -> targetItemsAcc -- FIXME: continue unboxing boxes?
    )
    targetItems


{-| Transfer function, Unboxing.
Reveals the content items (recursively) among the target items.
Returns the updated target items.
-}
unboxItems_ : TopicMap -> MapItems -> Model -> MapItems
unboxItems_ topicMap targetItems model =
  topicMap.items |> Dict.values |> List.filter TM.isVisible |> List.foldr
    (\mapItem targetItemsAcc ->
      case mapItem.props of
        TopicP _ ->
          let
            (items, abort) = unboxTopic mapItem targetItemsAcc model
          in
          if abort then
            items
          else
            case TM.byId mapItem.id model of
              Just topicMap_ -> unboxItems_ topicMap_ items model -- recursion
              Nothing -> items
        AssocP _ ->
          unboxAssoc mapItem targetItemsAcc
    )
    targetItems


{-| Returns the target item to reveal that corresponds to the box item.
Part of unboxing. FIXDOC
-}
unboxTopic : MapItem -> MapItems -> Model -> (MapItems, Bool)
unboxTopic mapItem targetItems model =
  let
    (topicToInsert, abort) =
      case targetItems |> Dict.get mapItem.id of
        Just item ->
          -- if box item exists (= was revealed before) ...
          -- 1) set it to "pinned" if visible already
          -- 2) abort further unboxing if it's display mode is BlackBox or WhiteBox
          let
            isPinned = if TM.isVisible item then Pinned else Unpinned
            newItem = { item | visibility = Visible isPinned }
            _ = U.info "unboxTopic" newItem
          in
          (newItem, isAbort item)
        Nothing ->
          -- by default (when no box item exists) an unboxed box will also be unboxed
          -- FIXME: set item's boxAssocId?
          if Item.isBox mapItem.id model then
            (setUnboxed mapItem, False)
          else
            (mapItem, False)
    assocToInsert = targetAssocItem -1 targetItems -- ### FIXME: -1 "mapItem.boxAssocId"
  in
  ( targetItems
    |> Dict.insert topicToInsert.id topicToInsert
    |> Dict.insert assocToInsert.id assocToInsert
  , abort
  )


unboxAssoc : MapItem -> MapItems -> MapItems
unboxAssoc mapItem targetItems =
  let
    assocToInsert = targetAssocItem mapItem.id targetItems
  in
  targetItems
    |> Dict.insert assocToInsert.id assocToInsert


setUnboxed : MapItem -> MapItem
setUnboxed item =
  { item | props =
    case item.props of
      TopicP props -> TopicP { props | displayMode = BoxD Unboxed }
      AssocP props -> AssocP props
  }


isAbort : MapItem -> Bool
isAbort item =
  case item.props of
    TopicP props ->
      case props.displayMode of
        BoxD BlackBox -> True
        BoxD WhiteBox -> True
        BoxD Unboxed -> False
        TopicD _ -> False
    AssocP _ -> False


{-| Returns the target item to reveal that corresponds to the box item.
Part of unboxing. FIXDOC
-}
targetAssocItem : Id -> MapItems -> MapItem
targetAssocItem assocId targetItems =
  case targetItems |> Dict.get assocId of
    Just item -> { item | visibility = Visible Unpinned } -- TODO: pinning?
    Nothing -> MapItem assocId (Visible Unpinned) (AssocP AssocProps)
