module Model.Reparent exposing (canReparent)

import AppModel as AM
import Domain.Reparent as R
import Model exposing (MapId)


canReparent : MapId -> Maybe MapId -> AM.Model -> Result String ()
canReparent a b model =
    R.canReparent a b (parentsOf model)


parentsOf : AM.Model -> MapId -> List MapId
parentsOf model childId =
    -- derive direct parent map IDs of childId from dmx.composition assocs
    -- TODO: implement using your existing assoc graph
    []
