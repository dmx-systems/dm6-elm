module Style exposing (..)

import Model exposing (..)
import Html exposing (Attribute)
import Html.Attributes exposing (style)



-- CONFIG


mainFontSize = "14px"

assocWith = 1.5
assocRadius = 14 -- should not bigger than half topic height
assocColor = "black"

topicSize = Size 128 28
topicIconSize = 16
topicBorderWidth = 1
topicRadius = 7

blackBoxOffset = 5

whiteBoxRange = Size 250 150
whiteBoxRadius = 14
whiteBoxPadding = 12



-- STYLE


appStyle : List (Attribute Msg)
appStyle =
  [ style "font-family" "sans-serif"
  , style "font-size" mainFontSize
  , style "user-select" "none"
  , style "-webkit-user-select" "none" -- Safari still needs vendor prefix
  ]


toolbarStyle : List (Attribute Msg)
toolbarStyle =
  [ style "display" "inline-flex"
  , style "flex-direction" "column"
  , style "align-items" "flex-start"
  , style "gap" "28px"
  , style "margin-top" "20px"
  , style "position" "relative"
  , style "z-index" "3"
  ]


displayModeStyle : Bool -> List (Attribute Msg)
displayModeStyle disabled =
  let
    (color, pointerEvents) =
      if disabled then
        ("gray", "none")
      else
        ("unset", "unset")
  in
  [ style "display" "flex"
  , style "flex-direction" "column"
  , style "gap" "6px"
  , style "color" color
  , style "pointer-events" pointerEvents
  ]


buttonStyle : List (Attribute Msg)
buttonStyle =
  [ style "font-family" "sans-serif"
  , style "font-size" mainFontSize
  ]


-- Edit Dialog

editDialogStyle : List (Attribute Msg)
editDialogStyle =
  [ style "position" "absolute"
  , style "left" "72px"
  , style "top" "244px"
  , style "width" "320px"
  , style "height" "320px"
  , style "background-color" "white"
  , style "border" "1px solid lightgray"
  ]


iconsListStyle : List (Attribute Msg)
iconsListStyle =
  [ style "height" "100%"
  , style "overflow" "auto"
  ]


iconButtonStyle : List (Attribute Msg)
iconButtonStyle =
  [ --style "background-color" "white"
    style "border-width" "0"
  , style "margin" "8px"
  ]


closeButtonStyle : List (Attribute Msg)
closeButtonStyle =
  [ style "position" "absolute"
  , style "top" "0"
  , style "right" "0"
  ]
