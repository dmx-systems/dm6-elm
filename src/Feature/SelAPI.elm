module Feature.SelAPI exposing (select, clear, isSelected, isSelectedPath, single,
  landingBoxPath)

import Box
import Feature.Sel exposing (Selection)
import Model exposing (Model)
import ModelParts exposing (..)
import Utils as U



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


single : Model -> Maybe (Id, BoxPath)
single model =
  case model.selection.items of
    [ selItem ] -> Just selItem
    _ -> Nothing


setItems : Selection -> Model -> Model
setItems items ({selection} as model) =
  { model | selection = { selection | items = items }}


--

{- The box where created things and search results land, entire box path, never empty.
Can be the fullsreen box or a nested box. -}
landingBoxPath : Model -> BoxPath
landingBoxPath model =
  case single model of
    Just (id, boxPath) ->
      let
        mapId = Box.firstId boxPath
      in
      case Box.displayMode id mapId model of
        Just (BoxD WhiteBox) -> id :: boxPath
        _ -> [ model.boxId ]
    Nothing -> [ model.boxId ]
