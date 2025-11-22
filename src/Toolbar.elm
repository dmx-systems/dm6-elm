module Toolbar exposing (view)

import Box
import Config as C
import Item
import Model exposing (Model, Msg(..), NavMsg(..))
import ModelHelper exposing (..)
import Tool
import Undo exposing (UndoModel)
import Utils as U
-- feature modules
import Icon
import IconAPI
import Search
import SearchAPI
import SelectionAPI as Sel
import TextEdit as T

import Html exposing (Html, Attribute, div, span, text, button, input, label, a)
import Html.Attributes exposing (href, style, type_, name, checked, disabled)
import Html.Events exposing (onClick)
import String exposing (fromInt)



-- VIEW


view : UndoModel -> Html Msg
view ({present} as undoModel) =
  div
    toolbarStyle
    [ div
      []
      [ Tool.viewButton "Edit" (Edit T.EditStart) Tool.hasSelection undoModel
      , Tool.viewButton "Set Icon" (Icon Icon.OpenMenu) Tool.hasSelection undoModel
      ]
    , Tool.viewButton "Traverse" (Search Search.ShowRelated) Tool.hasSelection undoModel
    , viewTopicDisplay present
    , viewBoxDisplay present
    , Tool.viewButton "Fullscreen" (Nav Fullscreen) Tool.hasBoxSelection undoModel
    , div
      []
      [ Tool.viewButton "Hide" Hide Tool.hasSelection undoModel
      , Tool.viewButton "Delete" Delete Tool.hasSelection undoModel
      ]
    , viewFooter
    ]


toolbarStyle : List (Attribute Msg)
toolbarStyle =
  [ style "font-size" <| fromInt C.toolFontSize ++ "px"
  , style "display" "flex"
  , style "flex-direction" "column"
  , style "align-items" "flex-start"
  , style "gap" "22px"
  , style "margin-top" "48px"
  , style "position" "fixed"
  , style "z-index" "1"
  ]


viewTopicDisplay : Model -> Html Msg
viewTopicDisplay model =
  let
    display = case Sel.single model of
      Just (topicId, boxPath) -> Box.displayMode topicId (Box.firstId boxPath) model.boxes
      Nothing -> Nothing
    (checked1, checked2, disabled_) =
      case display of
        Just (TopicD LabelOnly) -> (True, False, False)
        Just (TopicD Detail) -> (False, True, False)
        _ -> (False, False, True)
  in
  div
    (displayModeStyle disabled_)
    [ div
        []
        [ text "Topic Display" ]
    , viewRadioButton "Label Only" (SwitchDisplay <| TopicD LabelOnly) checked1 disabled_
    , viewRadioButton "Detail" (SwitchDisplay <| TopicD Detail) checked2 disabled_
    ]


viewBoxDisplay : Model -> Html Msg
viewBoxDisplay model =
  let
    display = case Sel.single model of
      Just (topicId, boxPath) -> Box.displayMode topicId (Box.firstId boxPath) model.boxes
      Nothing -> Nothing
    (checked1, checked2, checked3) =
      case display of
        Just (BoxD BlackBox) -> (True, False, False)
        Just (BoxD WhiteBox) -> (False, True, False)
        Just (BoxD Unboxed) -> (False, False, True)
        _ -> (False, False, False)
    disabled_ =
      case display of
        Just (BoxD _) -> False
        _ -> True
  in
  div
    (displayModeStyle disabled_)
    [ div
        []
        [ text "Box Display" ]
    , viewRadioButton "Black Box" (SwitchDisplay <| BoxD BlackBox) checked1 disabled_
    , viewRadioButton "White Box" (SwitchDisplay <| BoxD WhiteBox) checked2 disabled_
    , viewRadioButton "Unboxed" (SwitchDisplay <| BoxD Unboxed) checked3 disabled_
    ]


displayModeStyle : Bool -> List (Attribute Msg)
displayModeStyle disabled =
  let
    (color, pointerEvents) =
      if disabled then
        (C.disabledColor, "none")
      else
        ("unset", "unset")
  in
  [ style "display" "flex"
  , style "flex-direction" "column"
  , style "gap" "6px"
  , style "color" color
  , style "pointer-events" pointerEvents
  ]


viewRadioButton : String -> Msg -> Bool -> Bool -> Html Msg
viewRadioButton label_ msg isChecked isDisabled =
  label
    [ U.stopPropagationOnMousedown NoOp ]
    [ input
      [ type_ "radio", name "display-mode", checked isChecked, disabled isDisabled
      , onClick msg
      ]
      []
    , text label_
    ]


viewFooter : Html Msg
viewFooter =
  div
    footerStyle
    [ div
      []
      [ text C.version ]
    , div
      []
      [ text C.date ]
    , div
      []
      [ text "Source: "
      , a
        ( [ href "https://github.com/dmx-systems/dm6-elm" ]
          ++ linkStyle
        )
        [ text "GitHub" ]
      ]
    , a
      ( [ href "https://dmx.berlin" ]
        ++ linkStyle
      )
      [ text "DMX Systems" ]
    ]


footerStyle : List (Attribute Msg)
footerStyle =
  [ style "font-size" <| fromInt C.footerFontSize ++ "px"
  , style "color" "lightgray"
  ]


linkStyle : List (Attribute Msg)
linkStyle =
  [ style "color" "lightgray" ]
