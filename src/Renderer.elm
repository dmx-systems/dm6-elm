module Renderer exposing (viewSelect)

import Config as C
import Model exposing (Msg(..))
import ModelParts exposing (Attrs)
import RendererDef exposing (Renderer, decoder, toString)
import Utils as U

import Html exposing (Html, text, select, option)
import Html.Attributes exposing (style, value)
import Html.Events exposing (on, targetValue)
import Json.Decode as D
import String exposing (fromInt)



-- VIEW


viewSelect : Renderer -> (Renderer -> Msg) -> Html Msg
viewSelect renderer tagger =
  select
    ( [ value (toString renderer)
      , on "input" (D.map tagger (decoder targetValue))
      , U.onMouseDownStop NoOp
      ]
      ++ rendererSelectStyle
    )
    [ option [ value "TopicMap" ] [ text "Topic map" ]
    , option [ value "List" ] [ text "List" ]
    ]


rendererSelectStyle : Attrs Msg
rendererSelectStyle =
  [ style "position" "relative"
  , style "top" "-2px"
  , style "left" "3px"
  , style "font-family" C.mainFont
  , style "font-size" <| fromInt C.contentFontSize ++ "px"
  ]
