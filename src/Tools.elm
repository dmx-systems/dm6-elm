module Tools exposing (viewAppHeader)

import Config as C
import Model exposing (Model, Msg)

import Html exposing (Html, Attribute, div, text, input)
import Html.Attributes exposing (style)
import String exposing (fromInt)



viewAppHeader : Html Msg
viewAppHeader =
  div
    appHeaderStyle
    []


appHeaderStyle : List (Attribute Msg)
appHeaderStyle =
  [ style "height" <| fromInt C.appHeaderHeight ++ "px"
  , style "background-color" "black"
  , style "opacity" ".3"
  ]
