module Renderer exposing (viewSelect)

import Config as C
import Model exposing (Msg(..))
import ModelParts exposing (Attrs)
import Utils as U

import Html exposing (Html, text, select, option)
import Html.Attributes exposing (style)
import String exposing (fromInt)



-- VIEW


viewSelect : Html Msg
viewSelect =
  select
    ( [ U.onMouseDownStop NoOp ]
      ++ rendererSelectStyle
    )
    [ option [] [ text "Topic map" ]
    , option [] [ text "List" ]
    ]


rendererSelectStyle : Attrs Msg
rendererSelectStyle =
  [ style "position" "relative"
  , style "top" "-2px"
  , style "left" "3px"
  , style "font-family" C.mainFont
  , style "font-size" <| fromInt C.contentFontSize ++ "px"
  ]
