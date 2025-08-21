module Boxing exposing (boxContainer, unboxContainer)

import Dict
import Model exposing (..)



-- MODEL


type alias TransferFunc =
    MapItems -> MapItems -> Model -> MapItems



-- UPDATE


{-| Hides a container content from its parent map.
(Any target map can be given but de-facto it's the container's parent map)
-}
boxContainer : MapId -> MapId -> Model -> Maps
boxContainer containerId targetMapId model =
    transferContent containerId targetMapId boxItems model


{-| Reveals a container content on its parent map.
(Any target map can be given but de-facto it's the container's parent map)
-}
unboxContainer : MapId -> MapId -> Model -> Maps
unboxContainer containerId targetMapId model =
    transferContent containerId targetMapId unboxItems model


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
            (\containerItem targetItemsAcc ->
                let
                    items =
                        hideItem_ containerItem.id targetItemsAcc model
                in
                case getMapIfExists containerItem.id model.maps of
                    Just map ->
                        boxItems map.items items model

                    Nothing ->
                        items
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
                    -- abort further unboxing if view item exists (= was unboxed before) and is set to
                    -- BlackBox or WhiteBox
                    ( { item | hidden = False }, isAbort item )

                Nothing ->
                    -- by default (when no view item exists) an unboxed container will also be unboxed
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
            MapItem assocId False (MapAssoc AssocProps) -1
