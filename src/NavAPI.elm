module NavAPI exposing (update)

import Box
import Box.Size as Size
import Config as C
import Model exposing (Model, Msg)
import ModelHelper exposing (..)
import Utils as U
-- feature modules
import Nav
import SelectionAPI as Sel

import Browser.Navigation as Navigation
import String exposing (fromInt)



update : Nav.Msg -> Model -> Model
update msg model =
  case msg of
    Nav.Fullscreen -> fullscreen model
    Nav.Back -> back model
    Nav.UrlChanged url -> model
    Nav.LinkClicked urlRequest -> model


-- TODO
pushBoxUrl : BoxId -> Model -> Cmd Msg
pushBoxUrl boxId model =
  Navigation.pushUrl model.nav.key <| "/box/" ++ fromInt boxId


fullscreen : Model -> Model
fullscreen model =
  case Sel.single model of
    Just (topicId, _) ->
      { model | boxPath = topicId :: model.boxPath }
      |> Sel.clear
      |> adjustBoxRect topicId -1
    Nothing -> model


back : Model -> Model
back model =
  let
    (boxId, boxPath, selection) =
      case model.boxPath of
        prevBoxId :: nextBoxId :: boxIds ->
          ( prevBoxId
          , nextBoxId :: boxIds
          , [(prevBoxId, nextBoxId)]
          )
        _ -> U.logError "back" "model.boxPath has a problem" (0, [0], [])
  in
  { model
  | boxPath = boxPath
  -- , selection = selection -- TODO
  }
  |> adjustBoxRect boxId 1
  |> Size.auto


-- TODO
adjustBoxRect : BoxId -> Float -> Model -> Model
adjustBoxRect boxId factor model =
  model |> Box.updateRect boxId
    (\rect -> Rectangle
      (rect.x1 + factor * C.nestedBoxOffset.x)
      (rect.y1 + factor * C.nestedBoxOffset.y)
      rect.x2
      rect.y2
    )
