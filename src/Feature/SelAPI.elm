module Feature.SelAPI exposing (select, clear, isSelected, isSelectedPath, single,
  revelationBoxId, revelationBoxPath, landingBoxPath)

import Box
import Feature.Search exposing (SearchResult(..))
import Feature.Sel exposing (Selection)
import Model exposing (Model)
import ModelParts exposing (Id, BoxId, BoxPath, DisplayMode(..), BoxDisplay(..))
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


{- The box where to reveal search/traversal results, entire path -}
revelationBoxPath : Model -> Maybe BoxPath
revelationBoxPath model =
  case model.search.result of
    Topics _ _ ->
      Just <| landingBoxPath model
    RelTopics _ _ ->
      case single model of
        Just (_, boxPath) -> Just boxPath
        Nothing -> Nothing
    NoSearch -> Nothing


{- The box where both lands, 1) newly created things and 2) search results, entire path -}
landingBoxPath : Model -> BoxPath
landingBoxPath model =
  case single model of
    Just (id, boxPath) ->
      let
        boxId = Box.firstId boxPath
      in
      case Box.displayMode id boxId model of
        Just (BoxD WhiteBox) -> id :: boxPath
        _ -> [ model.boxId ]
    Nothing -> [ model.boxId ]


single : Model -> Maybe (Id, BoxPath)
single model =
  case model.selection.items of
    [ selItem ] -> Just selItem
    _ -> Nothing



-- PRIVATE


setItems : Selection -> Model -> Model
setItems items ({selection} as model) =
  { model | selection = { selection | items = items }}
