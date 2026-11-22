module Shared.Util exposing (requireIndex, findIndex)

import Console



requireIndex : n -> (h -> n) -> List h -> Maybe Int
requireIndex needle select list =
  case findIndex needle select list of
    Just index -> Just index
    Nothing ->
      Console.logError "Shared.Util.findIndex"
        (Console.toString needle ++ " not in " ++ Console.toString list)
        Nothing


findIndex : n -> (h -> n) -> List h -> Maybe Int
findIndex needle select list =
  let
    equality : Int -> h -> Maybe Int
    equality index haystack =
      case select haystack == needle of
        True -> Just index
        False -> Nothing


    result : List Int -> Maybe Int
    result indexes =
      case indexes of
        [ index ] -> Just index
        [] -> Nothing
        _ ->
          Console.logError "Shared.Util.findIndex"
            (Console.toString needle ++ " more than once in " ++ Console.toString list)
            Nothing
  in
  list
    |> List.indexedMap equality
    |> List.filterMap identity
    |> result
