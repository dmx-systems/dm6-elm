module TopicMap.Transfer exposing (boxContent, unboxContent)

import Item
import Model exposing (Model)
import ModelParts exposing (..)
import TopicMap.TopicMap as TM
import TopicMap.TopicMapDef exposing (MapItems, MapItem, Visibility(..), Pinned(..),
  ItemProps(..), AssocProps, DisplayMode(..), BoxDisplay(..))
import Utils as U

import Dict



-- MODEL


type alias Transfer = MapItems -> MapItems -> Model -> MapItems



-- UPDATE


{-| Removes box content from its parent box.
(Any target box can be given but de-facto it's the box's parent box.)
Presumtion: content is currently unboxed
-}
boxContent : BoxId -> BoxId -> Model -> Model
boxContent boxId targetBoxId model =
  transferContent boxId targetBoxId boxItems_ model


{-| Reveals box content on its parent box.
(Any target box can be given but de-facto it's the box's parent box.)
Presumtion: content is currently boxed (blackbox or whitebox)
-}
unboxContent : BoxId -> BoxId -> Model -> Model
unboxContent boxId targetBoxId model =
  transferContent boxId targetBoxId unboxItems_ model


transferContent : BoxId -> BoxId -> Transfer -> Model -> Model
transferContent boxId targetBoxId transfer model =
  case TM.byIdOrLog boxId model of
    Just box_ ->
      model |> TM.update targetBoxId
        (\targetBox ->
          { targetBox | items = transfer box_.items targetBox.items model }
        )
    Nothing -> model


{-| Transfer function, Boxing.
Removes the content items (recursively) from the target items.
Returns the updated target items.
-}
boxItems_ : MapItems -> MapItems -> Model -> MapItems
boxItems_ contentItems targetItems model =
  contentItems |> Dict.values |> List.filter TM.isVisible |> List.foldr
    (\boxItem targetItemsAcc ->
      case targetItemsAcc |> Dict.get boxItem.id of
        Just item ->
          if item |> TM.isPinned then
            -- don't box pinned items, only remove the assoc
            TM.removeItem_ boxItem.boxAssocId targetItemsAcc model
          else
            let
              items = TM.removeItem_ boxItem.id targetItemsAcc model
            in
            case TM.byId boxItem.id model of
              Just box_ -> boxItems_ box_.items items model -- recursion
              Nothing -> items
        Nothing -> targetItemsAcc -- FIXME: continue unboxing boxes?
    )
    targetItems


{-| Transfer function, Unboxing.
Reveals the content items (recursively) among the target items.
Returns the updated target items.
-}
unboxItems_ : MapItems -> MapItems -> Model -> MapItems
unboxItems_ contentItems targetItems model =
  contentItems |> Dict.values |> List.filter TM.isVisible |> List.foldr
    (\boxItem targetItemsAcc ->
      case boxItem.props of
        TopicP _ ->
          let
            (items, abort) = unboxTopic boxItem targetItemsAcc model
          in
          if abort then
            items
          else
            case TM.byId boxItem.id model of
              Just box_ -> unboxItems_ box_.items items model -- recursion
              Nothing -> items
        AssocP _ ->
          unboxAssoc boxItem targetItemsAcc
    )
    targetItems


{-| Returns the target item to reveal that corresponds to the box item.
Part of unboxing. FIXDOC
-}
unboxTopic : MapItem -> MapItems -> Model -> (MapItems, Bool)
unboxTopic boxItem targetItems model =
  let
    (topicToInsert, abort) =
      case targetItems |> Dict.get boxItem.id of
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
          if Item.isBox boxItem.id model then
            (setUnboxed boxItem, False)
          else
            (boxItem, False)
    assocToInsert = targetAssocItem boxItem.boxAssocId targetItems
  in
  ( targetItems
    |> Dict.insert topicToInsert.id topicToInsert
    |> Dict.insert assocToInsert.id assocToInsert
  , abort
  )


unboxAssoc : MapItem -> MapItems -> MapItems
unboxAssoc boxItem targetItems =
  let
    assocToInsert = targetAssocItem boxItem.id targetItems
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
    Nothing -> MapItem assocId -1 (Visible Unpinned) (AssocP AssocProps)
    -- FIXME: set item's boxAssocId?
