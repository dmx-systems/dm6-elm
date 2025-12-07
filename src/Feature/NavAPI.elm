module Feature.NavAPI exposing (boxIdFromUrl, pushUrl, update)

import Box
import Box.Size as Size
import Feature.Nav as Nav
import Feature.SelAPI as SelAPI
import Model exposing (Model, Msg(..))
import ModelParts exposing (..)
import Storage as S
import Undo exposing (UndoModel)
import Utils as U

import Browser.Dom as Dom
import Browser.Navigation as Navigation
import String exposing (fromInt, toInt)
import Url exposing (Url)
import Task



update : Nav.Msg -> UndoModel -> (UndoModel, Cmd Msg)
update msg ({present} as undoModel) =
  case msg of
    Nav.UrlChanged url -> urlChanged url undoModel
    Nav.LinkClicked urlRequest -> (undoModel, Cmd.none) -- TODO


urlChanged : Url -> UndoModel -> (UndoModel, Cmd Msg)
urlChanged url ({present} as undoModel) =
  case boxIdFromUrl url of
    Just boxId ->
      setFullscreenBox boxId present |> S.storeWith |> Undo.reset
    Nothing ->
      let
        _ = U.info "urlChanged" <| "No fragment -> redirect to " ++ fromInt present.boxId
      in
      (undoModel, pushUrl present.boxId present)


setFullscreenBox : BoxId -> Model -> (Model, Cmd Msg)
setFullscreenBox boxId model =
  let
    newModel = { model | boxId = boxId }
  in
  ( newModel
    |> SelAPI.clear -- TODO: add closeMenu etc. calls?
    |> Size.auto
  , setViewport newModel
  )


setViewport : Model -> Cmd Msg
setViewport model =
  case Box.byIdOrLog model.boxId model of
    Just box ->
      Dom.setViewportOf "main" box.scroll.x box.scroll.y
      |> Task.attempt
        (\result ->
          case result of
            Ok () -> NoOp
            Err e -> U.logError "setViewport" (U.toString e) NoOp
        )
    Nothing -> Cmd.none


{- Pushes a new box-URL. This results in rendering the given box fullscreen.
Note: the boxId is *not* necessarily the one contained in the model.
(The model is just used to access the navigation key.)
Eventually the model will be up-to-date (new boxId) as a *result* of the route change.
-}
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
