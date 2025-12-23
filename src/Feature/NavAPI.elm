port module Feature.NavAPI exposing (boxIdFromHash, pushUrl, update, sub)

import Box
import Box.Size as Size
import Feature.Nav as Nav
import Model exposing (Model, Msg(..))
import ModelParts exposing (..)
import Storage as S
import Undo exposing (UndoModel)
import Utils as U

import Browser.Dom as Dom
import String exposing (fromInt)
import Task



-- PORTS


port setHash : String -> Cmd msg

port onHashChange : (String -> msg) -> Sub msg



-- SUBSCRIPTIONS


sub : Sub Msg
sub =
  onHashChange (Nav << Nav.HashChanged)



-- UPDATE


update : Nav.Msg -> UndoModel -> (UndoModel, Cmd Msg)
update msg ({present} as undoModel) =
  case msg of
    Nav.HashChanged hash -> hashChanged hash undoModel


hashChanged : String -> UndoModel -> (UndoModel, Cmd Msg)
hashChanged hash ({present} as undoModel) =
  case boxIdFromHash hash of
    Just boxId ->
      setFullscreenBox boxId present |> S.storeWith |> Undo.reset
    Nothing ->
      let
        _ = U.info "hashChanged" <| "No hash -> redirect to " ++ fromInt present.boxId
      in
      (undoModel, pushUrl present.boxId)


setFullscreenBox : BoxId -> Model -> (Model, Cmd Msg)
setFullscreenBox boxId model =
  let
    newModel = { model | boxId = boxId }
  in
  ( newModel |> Size.auto
  , Cmd.batch
      [ setViewport newModel
      , U.command <| Cancel Nothing
      ]
  )


setViewport : Model -> Cmd Msg
setViewport model =
  case Box.byIdOrLog model.boxId model of
    Just box ->
      Dom.setViewportOf "main" (toFloat box.scroll.x) (toFloat box.scroll.y)
      |> Task.attempt
        (\result ->
          case result of
            Ok () -> NoOp
            Err e -> U.logError "setViewport" (U.toString e) NoOp
        )
    Nothing -> U.fail "setViewport" model.boxId Cmd.none


{- Pushes a new box-URL. This results in rendering the given box fullscreen.
Eventually the model.boxId will be updated, as a *result* of the route change.
-}
pushUrl : BoxId -> Cmd Msg
pushUrl boxId =
  setHash <| "#" ++ fromInt boxId


boxIdFromHash : String -> Maybe BoxId
boxIdFromHash hash =
  case String.isEmpty hash of
    True -> Nothing
    False ->
      case String.startsWith "#" hash of
        True -> case String.dropLeft 1 hash |> String.toInt of
          Just boxId -> Just boxId
          Nothing ->
            U.logError "boxIdFromHash" ("not a number after hash in \"" ++ hash ++ "\"") Nothing
        False -> U.logError "boxIdFromHash" ("\"" ++ hash ++ "\" is not a hash") Nothing
