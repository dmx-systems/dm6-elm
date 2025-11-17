module SelectionAPI exposing (select, reset, isSelected, single, singleBoxId)

import Model exposing (Model)
import ModelHelper exposing (Id, BoxId, BoxPath)
import Utils as U
-- feature modules
import Selection exposing (Selection)



-- PUBLIC API


select : Id -> BoxPath -> Model -> Model
select itemId boxPath model =
  let
    _ = U.info "select" (itemId, boxPath)
  in
  model
  |> setItems [ (itemId, boxPath) ]


reset : Model -> Model
reset model =
  model
  |> setItems []


isSelected : Id -> BoxId -> Model -> Bool
isSelected itemId boxId model =
  model.selection.items |> List.any
    (\(id, boxPath) ->
      case boxPath of
        boxId_ :: _ -> itemId == id && boxId == boxId_
        [] -> False
    )


single : Model -> Maybe (Id, BoxPath)
single model =
  case model.selection.items of
    [ selItem ] -> Just selItem
    _ -> Nothing


singleBoxId : Model -> Maybe BoxId
singleBoxId model =
  case single model of
    Just (_, boxPath) ->
      case boxPath of
        boxId :: _ -> Just boxId
        [] -> Nothing
    Nothing -> Nothing



-- PRIVATE


setItems : Selection -> Model -> Model
setItems items ({selection} as model) =
  { model | selection = { selection | items = items }}
