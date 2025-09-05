module Compat.CoreModel exposing
    ( CoreModel
    , empty
    , fromAppModel
    , toAppModel
    )

import AppModel as AM
import Dict
import Model exposing (..)



-- Minimal persistent model, independent of AppModel/UI.


type alias CoreModel =
    { items : Items
    , maps : Maps
    , mapPath : List MapId
    , nextId : Id
    }


empty : CoreModel
empty =
    { items = Dict.empty
    , maps = Dict.empty
    , mapPath = [ 0 ]
    , nextId = 1
    }



-- Project just the persistent bits from the full app model.


fromAppModel : AM.Model -> CoreModel
fromAppModel m =
    { items = m.items
    , maps = m.maps
    , mapPath = m.mapPath
    , nextId = m.nextId
    }



-- Lift a CoreModel into the full app model for rendering/storage.
-- Transient/UI fields are taken from AM.default.


toAppModel : CoreModel -> AM.Model
toAppModel c =
    { items = c.items
    , maps = c.maps
    , mapPath = c.mapPath
    , nextId = c.nextId
    , selection = AM.default.selection
    , editState = AM.default.editState
    , measureText = AM.default.measureText
    , mouse = AM.default.mouse
    , search = AM.default.search
    , iconMenu = AM.default.iconMenu
    , display = AM.default.display
    , fedWikiRaw = AM.default.fedWikiRaw
    }
