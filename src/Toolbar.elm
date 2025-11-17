module Toolbar exposing (view)

import Config as C
import Model exposing (Model, UndoModel, Msg(..))
import ModelAPI as A
import ModelHelper exposing (..)
import Utils as U
-- feature modules
import IconMenu
import IconMenuAPI
import Search
import SearchAPI
import TextEdit as T

import Html exposing (Html, Attribute, div, span, text, button, input, label, a)
import Html.Attributes exposing (href, style, type_, name, checked, disabled)
import Html.Events exposing (onClick)
import String exposing (fromInt)
import UndoList



-- VIEW


view : UndoModel -> Html Msg
view ({present} as undoModel) =
  div
    toolbarStyle
    [ viewMapNav present
    , SearchAPI.viewInput present
    , div
      []
      [ viewToolbarButton "Add Topic" AddTopic always undoModel
      , viewToolbarButton "Add Box" AddBox always undoModel
      ]
    , div
      []
      [ viewToolbarButton "Edit" (Edit T.EditStart) hasSelection undoModel
      , viewToolbarButton "Set Icon" (IconMenu IconMenu.Open) hasSelection undoModel
      ]
    , viewToolbarButton "Traverse" (Search Search.ShowRelated) hasSelection undoModel
    , viewMonadDisplay present
    , viewBoxDisplay present
    , viewToolbarButton "Fullscreen" (Nav Fullscreen) hasBoxSelection undoModel
    , div
      []
      [ viewToolbarButton "Hide" Hide hasSelection undoModel
      , viewToolbarButton "Delete" Delete hasSelection undoModel
      ]
    , div
      []
      [ viewToolbarButton "Undo" Undo hasPast undoModel
      , viewToolbarButton "Redo" Redo hasFuture undoModel
      ]
    , div
      []
      [ viewToolbarButton "Import" Import always undoModel
      , viewToolbarButton "Export" Export always undoModel
      ]
    , viewFooter
    ]


toolbarStyle : List (Attribute Msg)
toolbarStyle =
  [ style "font-size" <| fromInt C.toolbarFontSize ++ "px"
  , style "display" "flex"
  , style "flex-direction" "column"
  , style "align-items" "flex-start"
  , style "gap" "22px"
  , style "position" "fixed"
  , style "z-index" "1"
  ]


viewMapNav : Model -> Html Msg
viewMapNav model =
  let
    backDisabled = A.isAtRoot model
  in
  div
    mapNavStyle
    [ button
      [ onClick (Nav Back)
      , disabled backDisabled
      ]
      [ IconMenuAPI.viewIcon "arrow-left" 20 ]
    , span
      mapTitleStyle
      [ text <| getMapName model ]
    ]


mapNavStyle : List (Attribute Msg)
mapNavStyle =
  [ style "margin-top" "20px"
  , style "margin-bottom" "12px"
  ]


mapTitleStyle : List (Attribute Msg)
mapTitleStyle =
  [ style "font-size" "36px"
  , style "font-weight" "bold"
  , style "vertical-align" "top"
  , style "margin-left" "12px"
  ]


getMapName : Model -> String
getMapName model =
  case A.topicById (A.activeBox model) model of
    Just topic -> A.topicLabel topic
    Nothing -> "??"


viewToolbarButton : String -> Msg -> (UndoModel -> Bool) -> UndoModel -> Html Msg
viewToolbarButton label msg isEnabled undoModel =
  let
    buttonAttr =
      [ U.stopPropagationOnMousedown NoOp
      , disabled <| not <| isEnabled undoModel
      ]
  in
  button
    ( [ onClick msg ]
      ++ buttonAttr
      ++ buttonStyle
    )
    [ text label ]


{-| isEnabled predicate -}
hasSelection : UndoModel -> Bool
hasSelection undoModel =
  not (undoModel.present.selection |> List.isEmpty)


{-| isEnabled predicate -}
hasBoxSelection : UndoModel -> Bool
hasBoxSelection {present} =
  case A.singleSelection present of
    Just (id, _) -> A.isBox id present.boxes
    Nothing -> False


{-| isEnabled predicate -}
hasPast : UndoModel -> Bool
hasPast undoModel =
  undoModel |> UndoList.hasPast


{-| isEnabled predicate -}
hasFuture : UndoModel -> Bool
hasFuture undoModel =
  undoModel |> UndoList.hasFuture


{-| isEnabled predicate -}
always : UndoModel -> Bool
always undoModel =
  True


buttonStyle : List (Attribute Msg)
buttonStyle =
  [ style "font-family" C.mainFont
  , style "font-size" <| fromInt C.toolbarFontSize ++ "px"
  ]


viewMonadDisplay : Model -> Html Msg
viewMonadDisplay model =
  let
    display = case A.singleSelection model of
      Just (topicId, boxPath) -> A.displayMode topicId (A.firstId boxPath) model.boxes
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
    display = case A.singleSelection model of
      Just (topicId, boxPath) -> A.displayMode topicId (A.firstId boxPath) model.boxes
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
