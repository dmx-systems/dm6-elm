module Compat.Model exposing
    ( createAssoc
    , createAssocAndAddToMap
    , makeMap
    , makeMapItem
    , makeMapItemR
    , makeMapR
    )

{-| A tiny compatibility layer to isolate upstream refactors.
Use these helpers instead of calling constructors / ModelAPI directly.
-}

import Model exposing (..)
import ModelAPI



-- MAP ------------------------------------------------------------------------
-- Upstream (Sep 2025): Map id rect items


makeMap : MapId -> Rectangle -> MapItems -> Map
makeMap id rect items =
    Map id rect items


makeMapR r =
    Map r.id r.rect r.items



-- MAP ITEM -------------------------------------------------------------------


{-| Upstream (Aug 2025): MapItem id parentAssocId hidden pinned props
-}
makeMapItem id parentAssocId hidden pinned props =
    MapItem id parentAssocId hidden pinned props


makeMapItemR r =
    MapItem r.id r.parentAssocId r.hidden r.pinned r.props



-- ASSOC CREATION -------------------------------------------------------------


{-| Record-style wrapper; stable call-site even if upstream reorders args.
-}
createAssoc r model =
    -- Upstream: createAssoc itemType role1 player1 role2 player2 model
    ModelAPI.createAssoc r.itemType r.role1 r.player1 r.role2 r.player2 model


createAssocAndAddToMap r model =
    -- Upstream: createAssocAndAddToMap itemType role1 player1 role2 player2 mapId model
    ModelAPI.createAssocAndAddToMap r.itemType r.role1 r.player1 r.role2 r.player2 r.mapId model
