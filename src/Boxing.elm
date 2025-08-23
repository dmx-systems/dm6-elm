module Boxing exposing
    ( boxContainer
    , exitContainer
      -- NEW: the only new public API for now
    , unboxContainer
    )

-- keep these three helpers private for now
-- unboxTopicById : MapId -> Id -> MapId -> Model -> Maps
-- unboxToParent  : MapId -> Id -> Model -> Model
-- hideInContainer : MapId -> Id -> Model -> Model

import Dict
import Model exposing (..)
import Utils exposing (..)



-- MODEL


type alias TransferFunc =
    MapItems -> MapItems -> Model -> MapItems



-- UPDATE


{-| Hides a container content from its parent map.
(Any target map can be given but de-facto it's the container's parent map)
-}
boxContainer : MapId -> MapId -> Model -> Maps
boxContainer containerId targetMapId model =
    case getDisplayMode containerId targetMapId model.maps of
        -- box only if currently unboxed
        Just (Container Unboxed) ->
            transferContent containerId targetMapId boxItems model

        _ ->
            model.maps


{-| Reveals a container content on its parent map.
(Any target map can be given but de-facto it's the container's parent map)
-}
unboxContainer : MapId -> MapId -> Model -> Maps
unboxContainer containerId targetMapId model =
    case getDisplayMode containerId targetMapId model.maps of
        -- unbox only if currently boxed
        Just (Container BlackBox) ->
            transferContent containerId targetMapId unboxItems model

        Just (Container WhiteBox) ->
            transferContent containerId targetMapId unboxItems model

        _ ->
            model.maps


transferContent : MapId -> MapId -> TransferFunc -> Model -> Maps
transferContent containerId targetMapId transferFunc model =
    case getMap containerId model.maps of
        Just containerMap ->
            model.maps
                |> updateMaps
                    targetMapId
                    (\targetMap ->
                        { targetMap | items = transferFunc containerMap.items targetMap.items model }
                    )

        Nothing ->
            model.maps


{-| Transfer function, Boxing.
Iterates the container items (recursively) and sets corresponding target items to hidden.
Returns the updated target items.
-}
boxItems : MapItems -> MapItems -> Model -> MapItems
boxItems containerItems targetItems model =
    containerItems
        |> Dict.values
        |> List.foldr
            -- FIXME: apply isVisible filter?
            (\containerItem targetItemsAcc ->
                case targetItemsAcc |> Dict.get containerItem.id of
                    Just { pinned } ->
                        if pinned then
                            -- don't box pinned items, only hide the assoc
                            hideItem_ containerItem.parentAssocId targetItemsAcc model

                        else
                            let
                                items =
                                    hideItem_ containerItem.id targetItemsAcc model
                            in
                            case getMapIfExists containerItem.id model.maps of
                                Just map ->
                                    boxItems map.items items model

                                Nothing ->
                                    items

                    Nothing ->
                        targetItemsAcc
             -- FIXME: continue unboxing containers?
            )
            targetItems


{-| Transfer function, Unboxing.
Iterates the container items (recursively) and reveals corresponding target items.
Returns the updated target items.
-}
unboxItems : MapItems -> MapItems -> Model -> MapItems
unboxItems containerItems targetItems model =
    containerItems
        |> Dict.values
        |> List.filter isVisible
        |> List.foldr
            (\containerItem targetItemsAcc ->
                if isMapTopic containerItem then
                    -- TODO: use pattern matching instead?
                    let
                        ( items, abort ) =
                            unboxTopic containerItem targetItemsAcc model
                    in
                    if not abort then
                        case getMapIfExists containerItem.id model.maps of
                            Just map ->
                                unboxItems map.items items model

                            Nothing ->
                                items

                    else
                        items

                else
                    unboxAssoc containerItem targetItemsAcc
            )
            targetItems


{-| Returns the target item to reveal that corresponds to the container item.
Part of unboxing. FIXDOC
-}
unboxTopic : MapItem -> MapItems -> Model -> ( MapItems, Bool )
unboxTopic containerItem targetItems model =
    let
        ( topicToInsert, abort ) =
            case targetItems |> Dict.get containerItem.id of
                Just item ->
                    -- if map item exists (= was revealed before) ...
                    -- 1) set it to "pinned" unless it is hidden
                    -- 2) abort further unboxing if it's display mode is BlackBox or WhiteBox
                    let
                        _ =
                            info "unboxTopic" { item | hidden = False, pinned = not item.hidden }
                    in
                    ( { item | hidden = False, pinned = not item.hidden }, isAbort item )

                Nothing ->
                    -- by default (when no map item exists) an unboxed container will also be unboxed
                    -- FIXME: set item's parentAssocId?
                    if hasMap containerItem.id model.maps then
                        ( setUnboxed containerItem, False )

                    else
                        ( containerItem, False )

        assocToInsert =
            targetAssocItem containerItem.parentAssocId targetItems
    in
    ( targetItems
        |> Dict.insert topicToInsert.id topicToInsert
        |> Dict.insert assocToInsert.id assocToInsert
    , abort
    )


unboxAssoc : MapItem -> MapItems -> MapItems
unboxAssoc containerItem targetItems =
    let
        assocToInsert =
            targetAssocItem containerItem.id targetItems
    in
    targetItems
        |> Dict.insert assocToInsert.id assocToInsert


setUnboxed : MapItem -> MapItem
setUnboxed item =
    { item
        | props =
            case item.props of
                MapTopic props ->
                    MapTopic { props | displayMode = Container Unboxed }

                MapAssoc props ->
                    MapAssoc props
    }


isAbort : MapItem -> Bool
isAbort item =
    case item.props of
        MapTopic props ->
            case props.displayMode of
                Container BlackBox ->
                    True

                Container WhiteBox ->
                    True

                Container Unboxed ->
                    False

                Monad _ ->
                    False

        MapAssoc _ ->
            False


{-| Returns the target item to reveal that corresponds to the container item.
Part of unboxing. FIXDOC
-}
targetAssocItem : Id -> MapItems -> MapItem
targetAssocItem assocId targetItems =
    case targetItems |> Dict.get assocId of
        Just item ->
            { item | hidden = False }

        Nothing ->
            MapItem assocId False False (MapAssoc AssocProps) -1


{-| Move a single topic out of its container into the container’s parent map.
Semantics:

  - only acts if the topic exists and is visible in the container
  - respects BlackBox / WhiteBox abort via `unboxTopic`
  - if the topic has an inner map, unboxes its inner contents too
  - hides the original inside the container (so you don’t see a duplicate)

-}
exitContainer : MapId -> Id -> Model -> Model
exitContainer containerId topicId model =
    case getMap containerId model.maps of
        Nothing ->
            model

        Just containerMap ->
            let
                targetMapId =
                    containerMap.parentMapId

                targetItems0 =
                    case getMap targetMapId model.maps of
                        Just tm ->
                            tm.items

                        Nothing ->
                            Dict.empty
            in
            case Dict.get topicId containerMap.items of
                Nothing ->
                    model

                Just containerItem ->
                    if not (isVisible containerItem) then
                        model

                    else
                        let
                            -- reveal topic (and its assoc) in the target
                            ( targetItems1, abort ) =
                                unboxTopic containerItem targetItems0 model

                            -- if it’s a container and not aborted, reveal its inner contents too
                            targetItems2 =
                                if abort then
                                    targetItems1

                                else
                                    case getMapIfExists containerItem.id model.maps of
                                        Just innerMap ->
                                            unboxItems innerMap.items targetItems1 model

                                        Nothing ->
                                            targetItems1

                            -- write target map back
                            maps1 =
                                model.maps
                                    |> updateMaps targetMapId (\tm -> { tm | items = targetItems2 })

                            -- hide original inside the container (so it "left")
                            containerItems1 =
                                Dict.update topicId
                                    (Maybe.map (\it -> { it | hidden = True }))
                                    containerMap.items

                            maps2 =
                                maps1
                                    |> updateMaps containerId (\cm -> { cm | items = containerItems1 })
                        in
                        { model | maps = maps2 }
