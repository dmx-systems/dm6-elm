module Model.Invariant exposing (hasSelfContainment, offendingSelfContainers)

import Dict exposing (Dict)
import Model exposing (Map, MapId)


{-| Return all map ids that list themselves among their items.
-}
offendingSelfContainers : Dict MapId Map -> List MapId
offendingSelfContainers maps =
    maps
        |> Dict.foldl
            (\mapId m acc ->
                if Dict.member mapId m.items then
                    mapId :: acc

                else
                    acc
            )
            []
        |> List.reverse


{-| Convenience boolean.
-}
hasSelfContainment : Dict MapId Map -> Bool
hasSelfContainment maps =
    not (List.isEmpty (offendingSelfContainers maps))
