module Boxing exposing (box, unbox)

import AppModel exposing (..)
import Model exposing (..)
import ModelAPI as A
import Utils as U

import Dict



-- MODEL


type alias TransferFunc = BoxItems -> BoxItems -> Model -> BoxItems



-- UPDATE


{-| Hides box content from its parent box.
  (Any target box can be given but de-facto it's the box's parent box)
-}
box : BoxId -> BoxId -> Model -> Boxes
box boxId targetBoxId model =
  case A.displayMode boxId targetBoxId model.boxes of
    -- box only if currently unboxed
    Just (BoxD Unboxed) -> transferContent boxId targetBoxId boxItems_ model
    _ -> model.boxes


{-| Reveals box content on its parent box.
  (Any target box can be given but de-facto it's the box's parent box)
-}
unbox : BoxId -> BoxId -> Model -> Boxes
unbox boxId targetBoxId model =
  case A.displayMode boxId targetBoxId model.boxes of
    -- unbox only if currently boxed
    Just (BoxD BlackBox) -> transferContent boxId targetBoxId unboxItems_ model
    Just (BoxD WhiteBox) -> transferContent boxId targetBoxId unboxItems_ model
    _ -> model.boxes


transferContent : BoxId -> BoxId -> TransferFunc -> Model -> Boxes
transferContent boxId targetBoxId transferFunc model =
  case A.boxByIdOrLog boxId model.boxes of
    Just box_ ->
      model.boxes |> A.updateBoxes
        targetBoxId
        (\targetBox ->
          { targetBox | items = transferFunc box_.items targetBox.items model }
        )
    Nothing -> model.boxes


{-| Transfer function, Boxing.
Iterates the box items (recursively) and sets corresponding target items to hidden.
Returns the updated target items.
-}
boxItems_ : BoxItems -> BoxItems -> Model -> BoxItems
boxItems_ boxItems targetItems model =
  boxItems |> Dict.values |> List.foldr -- FIXME: apply isVisible filter?
    (\boxItem targetItemsAcc ->
      case targetItemsAcc |> Dict.get boxItem.id of
        Just {pinned} ->
          if pinned then
            -- don't box pinned items, only hide the assoc
            A.hideItem_ boxItem.parentAssocId targetItemsAcc model
          else
            let
              items = A.hideItem_ boxItem.id targetItemsAcc model
            in
            case A.boxById boxItem.id model.boxes of
              Just box_ -> boxItems_ box_.items items model -- recursion
              Nothing -> items
        Nothing -> targetItemsAcc -- FIXME: continue unboxing boxes?
    )
    targetItems


{-| Transfer function, Unboxing.
Iterates the box items (recursively) and reveals corresponding target items.
Returns the updated target items.
-}
unboxItems_ : BoxItems -> BoxItems -> Model -> BoxItems
unboxItems_ boxItems targetItems model =
  boxItems |> Dict.values |> List.filter A.isVisible |> List.foldr
    (\boxItem targetItemsAcc ->
      case boxItem.props of
        TopicV _ ->
          let
            (items, abort) = unboxTopic boxItem targetItemsAcc model
          in
          if abort then
            items
          else
            case A.boxById boxItem.id model.boxes of
              Just box_ -> unboxItems_ box_.items items model -- recursion
              Nothing -> items
        AssocV _ ->
          unboxAssoc boxItem targetItemsAcc
    )
    targetItems


{-| Returns the target item to reveal that corresponds to the box item.
Part of unboxing. FIXDOC
-}
unboxTopic : BoxItem -> BoxItems -> Model -> (BoxItems, Bool)
unboxTopic boxItem targetItems model =
  let
    (topicToInsert, abort) =
      case targetItems |> Dict.get boxItem.id of
        Just item ->
          -- if box item exists (= was revealed before) ...
          -- 1) set it to "pinned" unless it is hidden
          -- 2) abort further unboxing if it's display mode is BlackBox or WhiteBox
          let
            _ = U.info "unboxTopic" { item | hidden = False, pinned = not item.hidden }
          in
          ({ item | hidden = False, pinned = not item.hidden }, isAbort item)
        Nothing ->
          -- by default (when no box item exists) an unboxed box will also be unboxed
          -- FIXME: set item's parentAssocId?
          if A.isBox boxItem.id model.boxes then
            (setUnboxed boxItem, False)
          else
            (boxItem, False)
    assocToInsert = targetAssocItem boxItem.parentAssocId targetItems
  in
  ( targetItems
    |> Dict.insert topicToInsert.id topicToInsert
    |> Dict.insert assocToInsert.id assocToInsert
  , abort
  )


unboxAssoc : BoxItem -> BoxItems -> BoxItems
unboxAssoc boxItem targetItems =
  let
    assocToInsert = targetAssocItem boxItem.id targetItems
  in
  targetItems
    |> Dict.insert assocToInsert.id assocToInsert


setUnboxed : BoxItem -> BoxItem
setUnboxed item =
  { item | props =
    case item.props of
      TopicV props -> TopicV { props | displayMode = BoxD Unboxed }
      AssocV props -> AssocV props
  }


isAbort : BoxItem -> Bool
isAbort item =
  case item.props of
    TopicV props ->
      case props.displayMode of
        BoxD BlackBox -> True
        BoxD WhiteBox -> True
        BoxD Unboxed -> False
        TopicD _ -> False
    AssocV _ -> False


{-| Returns the target item to reveal that corresponds to the box item.
Part of unboxing. FIXDOC
-}
targetAssocItem : Id -> BoxItems -> BoxItem
targetAssocItem assocId targetItems =
  case targetItems |> Dict.get assocId of
    Just item -> { item | hidden = False }
    Nothing -> BoxItem assocId -1 False False (AssocV AssocProps) -- hidden/pinned=False
    -- FIXME: set item's parentAssocId?
