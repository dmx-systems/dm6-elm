module Model.Reparent exposing (canReparent, parentsOf)

import AppModel as AM
import Dict
import Domain.Reparent as DR
import Model exposing (..)


{-| Check whether `childId` may be reparented under `maybeNewParent`.

    * Fast guard against A→A self-containment.
    * Defers full cycle/ancestry validation to Domain.Reparent.canReparent,
      wiring it up with our local `parentsOf` lookup.

-}
canReparent : Id -> Maybe MapId -> AM.Model -> Result String ()
canReparent child maybeNewParent model =
    DR.canReparent child maybeNewParent (parentsOf model)


{-| Direct parent maps of a child, derived from `dmx.composition` assocs:

    itemType =
        "dmx.composition"

    role1 =
        "dmx.child" (player1 == child)

    role2 =
        "dmx.parent" (player2 == parent map id)

-}
parentsOf : AM.Model -> Id -> List MapId
parentsOf model childId =
    model.items
        |> Dict.values
        |> List.filterMap
            (\item ->
                case item.info of
                    Assoc assoc ->
                        if
                            assoc.itemType
                                == "dmx.composition"
                                && assoc.role1
                                == "dmx.child"
                                && assoc.role2
                                == "dmx.parent"
                                && assoc.player1
                                == childId
                        then
                            Just assoc.player2

                        else
                            Nothing

                    _ ->
                        Nothing
            )
