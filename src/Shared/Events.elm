module Shared.Events exposing (globalMouseHandler, itemClickHandler, draggable, onEsc,
  onEnterOrEsc, onClickStop, onPointerDownStop, onPointerOverStop, onPointerOutStop)

import Feature.MouseDef as MouseDef
import Model exposing (Msg(..))
import ModelBase exposing (..)

import Html exposing (Attribute)
import Html.Events exposing (on, stopPropagationOn, keyCode)
import Json.Decode as D



globalMouseHandler : Attrs Msg
globalMouseHandler =
  -- Topic Dragging. Note: dragging starts within the respective renderers. They attach
  -- pointerdown handlers to specific topics (using "topicDownHandler" utility above)
  [ on "pointermove" (D.map (Mouse << MouseDef.Move) pointDecoder)
  , on "pointerup" (D.succeed (Mouse MouseDef.Up))
  -- "Cancel UI"
  , on "pointerdown" (D.succeed (Mouse MouseDef.Cancel))
  ]


itemClickHandler : ItemId -> BoxPath -> Attrs Msg
itemClickHandler itemId boxPath =
  [ onClickStop <| ItemClicked itemId boxPath ]


draggable : TopicId -> BoxPath -> Attrs Msg
draggable topicId boxPath =
  [ stopPropagationWith "pointerdown"
      (D.map (Mouse << MouseDef.DragStart topicId boxPath) pointDecoder)
  ]


--

pointDecoder : D.Decoder (Point, PointerType)
pointDecoder =
  D.map2 Tuple.pair
    ( D.map2 Point
        (D.field "clientX" D.float |> D.map round)
        (D.field "clientY" D.float |> D.map round)
    )
    ( D.field "pointerType" D.string )


onEsc : Msg -> Attribute Msg
onEsc msg =
  on "keydown" (keyDecoder 27 msg)


onEnterOrEsc : Msg -> Attribute Msg
onEnterOrEsc msg =
  on "keydown"
    ( D.oneOf
        [ keyDecoder 13 msg
        , keyDecoder 27 msg
        ]
    )


keyDecoder : Int -> Msg -> D.Decoder Msg
keyDecoder key msg =
  let
    isKey code =
      if code == key then
        D.succeed msg
      else
        D.fail "not that key"
  in
  keyCode |> D.andThen isKey


onClickStop : Msg -> Attribute Msg
onClickStop msg =
  stopPropagation "click" msg


onPointerDownStop : Msg -> Attribute Msg
onPointerDownStop msg =
  stopPropagation "pointerdown" msg


onPointerOverStop : Msg -> Attribute Msg
onPointerOverStop msg =
  stopPropagation "pointerover" msg


onPointerOutStop : Msg -> Attribute Msg
onPointerOutStop msg =
  stopPropagation "pointerout" msg


stopPropagation : String -> Msg -> Attribute Msg
stopPropagation eventName msg =
  stopPropagationOn eventName <| D.succeed (msg, True) -- stopPropagation=True


stopPropagationWith : String -> D.Decoder Msg -> Attribute Msg
stopPropagationWith eventName decoder =
  stopPropagationOn eventName
    ( decoder
        |> D.map (\msg -> (msg, True)) -- stopPropagation=True
    )
