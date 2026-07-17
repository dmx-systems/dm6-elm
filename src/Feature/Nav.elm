port module Feature.Nav exposing (boxIdFromHash, pushUrl, update, sub)

import Box
import Env exposing (Env)
import Feature.NavDef as NavDef
import Model exposing (Model, Msg(..))
import ModelBase exposing (..)
import Outcome exposing (Outcome, Directives, Storage(..), History(..))
import TopicMap.TopicMap as TopicMap
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


update : NavDef.Msg -> Env -> Outcome
update msg env =
  case msg of
    NavDef.HashChanged hash ->
      env
        |> hashChanged hash


hashChanged : String -> Env -> Outcome
hashChanged hash ({model} as env) =
  case boxIdFromHash hash of
    Just boxId ->
      env
        |> setFullscreen boxId
        |> Outcome.newWith (Directives Store Reset)
    Nothing ->
      let
        _ = U.info "Feature.Nav.hashChanged"
          ("No hash -> redirect to " ++ fromInt (toBoxId model.boxId))
      in
      Outcome.with (pushUrl model.boxId) model


setFullscreen : BoxId -> Env -> (Model, Cmd Msg)
setFullscreen boxId ({model} as env) =
  let
    newModel = Box.setFullscreen boxId model
  in
  ( env
      |> Env.map (\_ -> newModel)
      |> Env.autoSize
      |> .model
  , Cmd.batch
      [ setViewport newModel
      , U.command (Cancel Nothing)
      ]
  )


setViewport : Model -> Cmd Msg
setViewport model =
  case TopicMap.fullscreen model of -- FIXME: dispatch instead
    Just topicMap ->
      Dom.setViewportOf "main" (toFloat topicMap.scroll.x) (toFloat topicMap.scroll.y)
        |> Task.attempt
          (\result ->
            case result of
              Ok () -> NoOp
              Err e -> U.logError "Feature.Nav.setViewport" (U.toString e) NoOp
          )
    Nothing ->
      U.fail "Feature.Nav.setViewport" model.boxId Cmd.none


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
            U.logError "Feature.Nav.boxIdFromHash"
              ("not a number after hash in \"" ++ hash ++ "\"") Nothing
        False ->
          U.logError "Feature.Nav.boxIdFromHash" ("\"" ++ hash ++ "\" is not a hash") Nothing
