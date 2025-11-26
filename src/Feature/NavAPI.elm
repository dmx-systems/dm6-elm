module Feature.NavAPI exposing (boxIdFromUrl, pushUrl, update)

import Feature.Nav as Nav
import Feature.SelectionAPI as Sel
import Model exposing (Model, Msg)
import ModelParts exposing (..)
import Storage as S
import Undo exposing (UndoModel)
import Utils as U

import Browser.Navigation as Navigation
import String exposing (fromInt, toInt)
import Url exposing (Url)



update : Nav.Msg -> UndoModel -> (UndoModel, Cmd Msg)
update msg ({present} as undoModel) =
  case msg of
    Nav.Fullscreen -> (undoModel, fullscreenRoute present)
    Nav.UrlChanged url -> urlChanged url undoModel
    Nav.LinkClicked urlRequest -> (undoModel, Cmd.none) -- TODO


fullscreenRoute : Model -> Cmd Msg
fullscreenRoute model =
  case Sel.single model of
    Just (boxId, _) -> pushUrl boxId model
    Nothing -> Cmd.none


urlChanged : Url -> UndoModel -> (UndoModel, Cmd Msg)
urlChanged url ({present} as undoModel) =
  case boxIdFromUrl url of
    Just boxId ->
      fullscreen boxId present |> S.store |> Undo.reset
    Nothing ->
      let
        _ = U.info "urlChanged" <| "No fragment -> redirect to " ++ fromInt present.boxId
      in
      (undoModel, pushUrl present.boxId present)


fullscreen : BoxId -> Model -> Model
fullscreen boxId model =
  { model | boxId = boxId }
  |> Sel.clear


pushUrl : BoxId -> Model -> Cmd Msg
pushUrl boxId model =
  Navigation.pushUrl model.nav.key <| "#" ++ fromInt boxId


boxIdFromUrl : Url -> Maybe BoxId
boxIdFromUrl url =
  case url.fragment of
    Just str ->
      case toInt str of
        Just boxId -> Just boxId
        Nothing ->
          U.logError "boxIdFromUrl" ("\"" ++ str ++ "\" is not a number") Nothing
    Nothing -> Nothing
