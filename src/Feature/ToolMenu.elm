module Feature.ToolMenu exposing (view)

import Config as C
import Feature.ToolDef as ToolDef exposing (LineStyle(..))
import Model exposing (Model, Msg(..))
import ModelBase exposing (..)
import Shared.Events as Events
import Shared.ViewBase as VB

import Html exposing (Html, div, text, button, input, label, a)
import Html.Attributes exposing (style, name, type_, checked, href)
import Html.Events exposing (onClick)
import String exposing (fromInt)



view : Model -> List (Html Msg)
view model =
  let
    is = (==) model.tool.lineStyle
  in
  if model.tool.menu then
    [ div
        menuStyle
        [ div headingStyle [ text "Line Style" ]
        , div
            []
            [ viewRadioButton "Cornered" (ToolDef.Set Cornered) <| is Cornered
            , VB.hGap 20
            , viewRadioButton "Straight" (ToolDef.Set Straight) <| is Straight
            ]
        , VB.vGap 32
        , div headingStyle [ text "Database" ]
        , div
            []
            [ viewTextButton "Import" ToolDef.Import
            , VB.hGap 20
            , viewTextButton "Export" ToolDef.Export
            ]
        , VB.vGap 32
        , div headingStyle [ text "About" ]
        , viewAbout
        ]
    ]
  else
    []


menuStyle : Attrs Msg
menuStyle =
  [ style "font-size" <| fromInt C.toolFontSize ++ "px"
  , style "position" "absolute"
  , style "top" "32px"
  , style "right" "10px"
  , style "border" "1px solid lightgray"
  , style "background-color" "white"
  , style "padding" "24px 18px 16px"
  , style "z-index" "5"
  ]


viewRadioButton : String -> ToolDef.Msg -> Bool -> Html Msg
viewRadioButton label_ msg isChecked  =
  label
    [ Events.onPointerDownStop NoOp ]
    [ input
      ( [ type_ "radio"
        , name "line-style"
        , checked isChecked
        , onClick <| Tool msg
        ]
        ++ radioButtonStyle
      )
      []
    , text label_
    ]


viewTextButton : String -> ToolDef.Msg -> Html Msg
viewTextButton label msg =
  button
    ( [ onClick <| Tool msg
      , Events.onPointerDownStop NoOp
      ]
      ++ textButtonStyle
    )
    [ text label ]


headingStyle : Attrs Msg
headingStyle =
  [ style "font-weight" "bold"
  , style "margin-bottom" "14px"
  ]


radioButtonStyle : Attrs Msg
radioButtonStyle =
  [ style "margin" "0 6px 0 0" ]


textButtonStyle : Attrs Msg
textButtonStyle =
  [ style "font-family" C.mainFont
  , style "font-size" <| fromInt C.toolFontSize ++ "px"
  ]


-- About

viewAbout : Html Msg
viewAbout =
  div
    aboutStyle
    [ div
        []
        [ text ("DM6 Elm " ++ C.version) ]
    , div
        []
        [ text ("Released: " ++ C.date) ]
    , div
        []
        [ text "Source code: "
        , a
            ( [ href "https://github.com/dmx-systems/dm6-elm" ]
              ++ linkStyle
            )
            [ text "GitHub" ]
        ]
    , div
        []
        [ text "Website: "
        , a
            ( [ href "https://dmx.berlin" ]
              ++ linkStyle
            )
            [ text "DMX Berlin" ]
        ]
    ]


aboutStyle : Attrs Msg
aboutStyle =
  [ style "font-family" C.mainFont
  , style "font-size" <| fromInt C.aboutFontSize ++ "px"
  , style "line-height" "1.3"
  , style "color" "gray"
  ]


linkStyle : Attrs Msg
linkStyle =
  [ style "color" "gray" ]
