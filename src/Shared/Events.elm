module Shared.Events exposing (..)

import Model exposing (Msg(..))
import ModelBase exposing (..)
import TopicMap.TopicMapDef as TopicMapDef -- TODO: drop

import Html exposing (Attribute)
import Html.Events exposing (on, stopPropagationOn, keyCode)
import Json.Decode as D



topicDownHandler : TopicId -> BoxPath -> Attrs Msg
topicDownHandler topicId boxPath =
  [ stopPropagationWith "pointerdown"
      ( pointDecoder |> D.map
          (TopicMap << TopicMapDef.DownOnTopic topicId boxPath)
      )
  ]


itemClickHandler : ItemId -> BoxPath -> Attrs Msg
itemClickHandler itemId boxPath =
  [ onClickStop <| ItemClicked itemId boxPath ]


dragHandler : Attrs Msg
dragHandler =
  -- "Cancel UI"
  [ on "pointerdown" <| D.succeed <| TopicMap TopicMapDef.Down
  -- Topic Dragging. Note: dragging starts within the respective renderers. They attach
  -- pointerdown handlers to specific topics (using "topicDownHandler" utility above)
  , on "pointermove" <| D.map (TopicMap << TopicMapDef.Move) pointDecoder
  , on "pointerup" <| D.succeed <| TopicMap TopicMapDef.Up
  ]


--

onEsc : msg -> Attribute msg
onEsc msg =
  on "keydown" (keyDecoder 27 msg)


onEnterOrEsc : msg -> Attribute msg
onEnterOrEsc msg =
  on "keydown"
    ( D.oneOf
      [ keyDecoder 13 msg
      , keyDecoder 27 msg
      ]
    )


keyDecoder : Int -> msg -> D.Decoder msg
keyDecoder key msg =
  let
    isKey code =
      if code == key then
        D.succeed msg
      else
        D.fail "not that key"
  in
  keyCode |> D.andThen isKey


onClickStop : msg -> Attribute msg
onClickStop msg =
  stopPropagation "click" msg


onPointerDownStop : msg -> Attribute msg
onPointerDownStop msg =
  stopPropagation "pointerdown" msg


onPointerOverStop : msg -> Attribute msg
onPointerOverStop msg =
  stopPropagation "pointerover" msg


onPointerOutStop : msg -> Attribute msg
onPointerOutStop msg =
  stopPropagation "pointerout" msg


stopPropagation : String -> msg -> Attribute msg
stopPropagation eventName msg =
  stopPropagationOn eventName <| D.succeed (msg, True) -- stopPropagation=True


stopPropagationWith : String -> D.Decoder msg -> Attribute msg
stopPropagationWith eventName decoder =
  stopPropagationOn eventName
    ( decoder
        |> D.map (\msg -> (msg, True)) -- stopPropagation=True
    )



-- DECODER


pointDecoder : D.Decoder (Point, PointerType)
pointDecoder =
  D.map2 Tuple.pair
    ( D.map2 Point
        (D.field "clientX" D.float |> D.map round)
        (D.field "clientY" D.float |> D.map round)
    )
    ( D.field "pointerType" D.string )
