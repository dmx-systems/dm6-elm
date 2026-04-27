module Feature.Sel exposing (select, clear, isSelected, single, landingBoxPath)

import Box
import Feature.SelDef exposing (Selection)
import Model exposing (Model)
import ModelBase exposing (..)
import Topic
import Utils as U



select : ItemId -> BoxPath -> Model -> Model
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


isSelected : ItemId -> BoxPath -> Model -> Bool
isSelected itemId boxPath model =
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
    Just (T (TopicId id as topicId), boxPath) ->
      let
        boxId = Box.firstId boxPath
        isExpanded = Box.expansionOf topicId boxId model == Expanded
      in
      if Topic.isBox id model && isExpanded then
        id :: boxPath
      else
        [ model.boxId ]
    _ -> [ model.boxId ]
