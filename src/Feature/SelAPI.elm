module Feature.SelAPI exposing (select, clear, isSelected, isSelectedPath, single,
  revelationBoxId, revelationBoxPath)

import Feature.Search exposing (SearchResult(..))
import Feature.Sel exposing (Selection)
import Item
import Model exposing (Model)
import ModelParts exposing (Id, BoxId, BoxPath)
import Utils as U



-- PUBLIC API


select : Id -> BoxPath -> Model -> Model
select itemId boxPath model =
  let
    _ = U.info "select" (itemId, boxPath)
  in
  model
  |> setItems [ (itemId, boxPath) ]


clear : Model -> Model
clear model =
  model
  |> setItems []


-- TODO: drop this in favor of isSelectedPath (and rename the latter then)?
isSelected : Id -> BoxId -> Model -> Bool
isSelected itemId boxId model =
  model.selection.items |> List.any
    (\(id, boxPath) ->
      case boxPath of
        boxId_ :: _ -> itemId == id && boxId == boxId_
        [] -> False
    )


isSelectedPath : Id -> BoxPath -> Model -> Bool
isSelectedPath itemId boxPath model =
  model.selection.items
  |> List.member (itemId, boxPath)


{- The box where to reveal search/traversal results -}
revelationBoxId : Model -> Maybe BoxId
revelationBoxId model =
  case revelationBoxPath model of
    Just (boxId :: _) -> Just boxId
    _ -> Nothing


revelationBoxPath : Model -> Maybe BoxPath
revelationBoxPath model =
  case model.search.result of
    Topics _ _ ->
      case single model of
        Just (id, boxPath) ->
          case Item.isBox id model of
            True -> Just (id :: boxPath)
            False -> Just [ model.boxId ]
        Nothing -> Just [ model.boxId ]
    RelTopics _ _ ->
      case single model of
        Just (_, boxPath) -> Just boxPath
        Nothing -> Nothing
    NoSearch -> Nothing


single : Model -> Maybe (Id, BoxPath)
single model =
  case model.selection.items of
    [ selItem ] -> Just selItem
    _ -> Nothing



-- PRIVATE


setItems : Selection -> Model -> Model
setItems items ({selection} as model) =
  { model | selection = { selection | items = items }}
