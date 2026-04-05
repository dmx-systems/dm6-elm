module Renderer exposing (viewSelect)

import Config as C
import Model exposing (Msg(..))
import ModelParts exposing (Attrs)
import RendererDef exposing (Renderer, all, decoder, toName)
import Utils as U

import Dict
import Html exposing (Html, text, select, option)
import Html.Attributes exposing (style, value)
import Html.Events exposing (on, targetValue)
import Json.Decode as D
import String exposing (fromInt)



-- VIEW


viewSelect : Renderer -> (Renderer -> Msg) -> Html Msg
viewSelect renderer tagger =
  select
    ( [ value (toName renderer)
      , on "input" (D.map tagger (decoder targetValue))
      , U.onMouseDownStop NoOp
      ]
      ++ selectStyle
    )
    viewOptions


viewOptions : List (Html Msg)
viewOptions =
  all
    |> Dict.values
    |> List.map
      (\{name, label} ->
        option
          [ value name ]
          [ text label ]
      )


selectStyle : Attrs Msg
selectStyle =
  [ style "position" "relative"
  , style "top" "-2px"
  , style "left" "3px"
  , style "font-family" C.mainFont
  , style "font-size" <| fromInt C.contentFontSize ++ "px"
  ]
