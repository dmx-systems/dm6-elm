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
    Nav.Fullscreen -> (undoModel, pushBoxUrl present)
    Nav.UrlChanged url -> urlChanged url present |> S.store |> Undo.reset
    Nav.LinkClicked urlRequest -> (undoModel, Cmd.none) -- TODO


pushBoxUrl : Model -> Cmd Msg
pushBoxUrl model =
  case Sel.single model of
    Just (boxId, _) ->
      Navigation.pushUrl model.nav.key <| "#" ++ fromInt boxId
    Nothing -> Cmd.none


urlChanged : Url -> Model -> Model
urlChanged url model =
  case url.fragment of
    Just str ->
      case toInt str of
        Just boxId -> fullscreen boxId model
        Nothing -> U.logError "urlChanged" ("\"" ++ str ++ "\" is not a number") model
    Nothing -> U.logError "urlChanged" "URL misses fragment" model


fullscreen : BoxId -> Model -> Model
fullscreen boxId model =
  { model | boxPath = boxId :: model.boxPath }
  |> Sel.clear
