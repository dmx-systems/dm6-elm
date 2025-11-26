module NavAPI exposing (update)

import Model exposing (Model, Msg)
import ModelHelper exposing (..)
import Storage as S
import Undo exposing (UndoModel)
import Utils as U
-- feature modules
import Nav
import SelectionAPI as Sel

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
  case url.fragment of
    Just str ->
      case toInt str of
        Just boxId ->
          fullscreen boxId present |> S.store |> Undo.reset
        Nothing ->
          U.logError "urlChanged" ("\"" ++ str ++ "\" is not a number")
          (undoModel, Cmd.none) -- TODO
    Nothing ->
      let
        _ = U.info "urlChanged" "URL w/o fragment -> redirect to root box"
      in
      (undoModel, pushUrl rootBoxId present)


fullscreen : BoxId -> Model -> Model
fullscreen boxId model =
  { model | boxId = boxId }
  |> Sel.clear


pushUrl : BoxId -> Model -> Cmd Msg
pushUrl boxId model =
  Navigation.pushUrl model.nav.key <| "#" ++ fromInt boxId
