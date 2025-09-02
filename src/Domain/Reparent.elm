module Domain.Reparent exposing (canReparent, isDescendant)

{-| Pure reparenting rules. This module doesn’t know about your Model.
You pass `parentsOf : id -> List id` at the call site (or wrap it).
-}


canReparent :
    comparable -- A
    -> Maybe comparable -- target B (Nothing = root)
    -> (comparable -> List comparable) -- parentsOf
    -> Result String ()
canReparent a maybeB parentsOf =
    case maybeB of
        Nothing ->
            Ok ()

        Just b ->
            if a == b then
                Err "Cannot make a map contain itself."

            else if isDescendant b a parentsOf then
                Err "Cannot move a map under its own descendant (cycle)."

            else
                Ok ()


{-| True if `x` is a (strict) descendant of `y`.
-}
isDescendant : comparable -> comparable -> (comparable -> List comparable) -> Bool
isDescendant x y parentsOf =
    -- y ∈ ancestors(x) ?
    ancestors x parentsOf |> List.member y


ancestors : comparable -> (comparable -> List comparable) -> List comparable
ancestors m parentsOf =
    let
        go seen frontier =
            case frontier of
                [] ->
                    seen

                p :: ps ->
                    if List.member p seen then
                        go seen ps

                    else
                        go (p :: seen) (parentsOf p ++ ps)
    in
    go [] (parentsOf m)
