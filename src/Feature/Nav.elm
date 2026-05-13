port module Feature.Nav exposing (boxIdFromHash, pushUrl, update, sub)

import Env exposing (Env)
import Feature.NavDef as NavDef
import Model exposing (Model, Msg(..))
import ModelBase exposing (..)
import Storage as S
import TopicMap.BoxProps as TM
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
  onHashChange (Nav << NavDef.HashChanged)



-- UPDATE


update : NavDef.Msg -> Env -> (UndoModel, Cmd Msg)
update msg env =
  case msg of
    NavDef.HashChanged hash -> hashChanged hash env


hashChanged : String -> Env -> (UndoModel, Cmd Msg)
hashChanged hash ({model, undoModel} as env) =
  case boxIdFromHash hash of
    Just boxId ->
      setFullscreenBox boxId env |> S.storeWith |> Undo.reset
    Nothing ->
      let
        _ = U.info "hashChanged" <| "No hash -> redirect to " ++ fromInt (toBoxId model.boxId)
      in
      (undoModel, pushUrl model.boxId)


setFullscreenBox : BoxId -> Env -> (Model, Cmd Msg)
setFullscreenBox boxId ({model} as env) =
  let
    newModel = { model | boxId = boxId }
  in
  ( newModel
      |> Env.autoSize env
  , Cmd.batch
      [ setViewport newModel
      , U.command <| Cancel Nothing
      ]
  )


setViewport : Model -> Cmd Msg
setViewport model =
  case TM.fullscreen model of
    Just map ->
      Dom.setViewportOf "main" (toFloat map.scroll.x) (toFloat map.scroll.y)
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
  setHash <| "#" ++ fromInt (toBoxId boxId)


boxIdFromHash : String -> Maybe BoxId
boxIdFromHash hash =
  case String.isEmpty hash of
    True -> Nothing
    False ->
      case String.startsWith "#" hash of
        True -> case String.dropLeft 1 hash |> String.toInt of
          Just boxId -> Just (BoxId (TopicId boxId))
          Nothing ->
            U.logError "boxIdFromHash" ("not a number after hash in \"" ++ hash ++ "\"") Nothing
        False -> U.logError "boxIdFromHash" ("\"" ++ hash ++ "\" is not a hash") Nothing
