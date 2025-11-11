module Boxing exposing (box, unbox)

import AppModel exposing (..)
import Model exposing (..)
import ModelAPI exposing (..)
import Utils exposing (..)

import Dict



-- MODEL


type alias TransferFunc = MapItems -> MapItems -> Model -> MapItems



-- UPDATE


{-| Hides box content from its parent map.
  (Any target map can be given but de-facto it's the box's parent map)
-}
box : BoxId -> BoxId -> Model -> Boxes
box boxId targetMapId model =
  case displayMode boxId targetMapId model.boxes of
    -- box only if currently unboxed
    Just (Box Unboxed) -> transferContent boxId targetMapId boxItems_ model
    _ -> model.boxes


{-| Reveals box content on its parent map.
  (Any target map can be given but de-facto it's the box's parent map)
-}
unbox : BoxId -> BoxId -> Model -> Boxes
unbox boxId targetMapId model =
  case displayMode boxId targetMapId model.boxes of
    -- unbox only if currently boxed
    Just (Box BlackBox) -> transferContent boxId targetMapId unboxItems_ model
    Just (Box WhiteBox) -> transferContent boxId targetMapId unboxItems_ model
    _ -> model.boxes


transferContent : BoxId -> BoxId -> TransferFunc -> Model -> Boxes
transferContent boxId targetMapId transferFunc model =
  case mapByIdOrLog boxId model.boxes of
    Just map ->
      model.boxes |> updateMaps
        targetMapId
        (\targetMap ->
          { targetMap | items = transferFunc map.items targetMap.items model }
        )
    Nothing -> model.boxes


{-| Transfer function, Boxing.
Iterates the box items (recursively) and sets corresponding target items to hidden.
Returns the updated target items.
-}
boxItems_ : MapItems -> MapItems -> Model -> MapItems
boxItems_ boxItems targetItems model =
  boxItems |> Dict.values |> List.foldr -- FIXME: apply isVisible filter?
    (\boxItem targetItemsAcc ->
      case targetItemsAcc |> Dict.get boxItem.id of
        Just {pinned} ->
          if pinned then
            -- don't box pinned items, only hide the assoc
            hideItem_ boxItem.parentAssocId targetItemsAcc model
          else
            let
              items = hideItem_ boxItem.id targetItemsAcc model
            in
            case mapById boxItem.id model.boxes of
              Just map -> boxItems_ map.items items model -- recursion
              Nothing -> items
        Nothing -> targetItemsAcc -- FIXME: continue unboxing boxes?
    )
    targetItems


{-| Transfer function, Unboxing.
Iterates the box items (recursively) and reveals corresponding target items.
Returns the updated target items.
-}
unboxItems_ : MapItems -> MapItems -> Model -> MapItems
unboxItems_ boxItems targetItems model =
  boxItems |> Dict.values |> List.filter isVisible |> List.foldr
    (\boxItem targetItemsAcc ->
      case boxItem.props of
        MapTopic _ ->
          let
            (items, abort) = unboxTopic boxItem targetItemsAcc model
          in
          if abort then
            items
          else
            case mapById boxItem.id model.boxes of
              Just map -> unboxItems_ map.items items model -- recursion
              Nothing -> items
        MapAssoc _ ->
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
          -- if map item exists (= was revealed before) ...
          -- 1) set it to "pinned" unless it is hidden
          -- 2) abort further unboxing if it's display mode is BlackBox or WhiteBox
          let
            _ = info "unboxTopic" { item | hidden = False, pinned = not item.hidden }
          in
          ({ item | hidden = False, pinned = not item.hidden }, isAbort item)
        Nothing ->
          -- by default (when no map item exists) an unboxed box will also be unboxed
          -- FIXME: set item's parentAssocId?
          if hasMap boxItem.id model.boxes then
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
      MapTopic props -> MapTopic { props | displayMode = Box Unboxed }
      MapAssoc props -> MapAssoc props
  }


isAbort : MapItem -> Bool
isAbort item =
  case item.props of
    MapTopic props ->
      case props.displayMode of
        Box BlackBox -> True
        Box WhiteBox -> True
        Box Unboxed -> False
        Monad _ -> False
    MapAssoc _ -> False


{-| Returns the target item to reveal that corresponds to the box item.
Part of unboxing. FIXDOC
-}
targetAssocItem : Id -> MapItems -> MapItem
targetAssocItem assocId targetItems =
  case targetItems |> Dict.get assocId of
    Just item -> { item | hidden = False }
    Nothing -> MapItem assocId -1 False False (MapAssoc AssocProps) -- hidden/pinned=False
    -- FIXME: set item's parentAssocId?
