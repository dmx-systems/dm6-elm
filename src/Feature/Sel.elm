module Feature.Sel exposing (select, clear, isSelected, isSelectedPath, single,
  landingBoxPath)

import Box
import Feature.SelDef exposing (Selection)
import Item
import Model exposing (Model)
import ModelBase exposing (..)
import Utils as U



select : Id -> BoxPath -> Model -> Model
select itemId boxPath model =
  let
    _ = U.info "Feature.Sel.select" (itemId, boxPath)
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


single : Model -> Maybe Target
single model =
  case model.selection.items of
    [ selItem ] -> Just selItem
    _ -> Nothing


setItems : Selection -> Model -> Model
setItems items ({selection} as model) =
  { model | selection = { selection | items = items }}


--

{- The box where created things and search results land, entire box path, never empty.
Can be the fullscreen box or a nested box.
-}
landingBoxPath : Model -> BoxPath
landingBoxPath model =
  case single model of
    Just (id, boxPath) ->
      let
        boxId = Box.firstId boxPath
        isExpanded = Box.expansionOf id boxId model == Expanded
      in
      if Item.isBox id model && isExpanded then
        id :: boxPath
      else
        [ model.boxId ]
    Nothing -> [ model.boxId ]
