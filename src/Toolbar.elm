module Toolbar exposing (viewToolbar)

import AppModel exposing (UndoModel, Model, Msg(..))
import Config as C
import Model exposing (EditMsg(..), NavMsg(..), DisplayMode(..), MonadDisplay(..),
  ContainerDisplay(..))
import ModelAPI exposing (topicById, topicLabel, firstId, isHome, activeMap,
  displayMode, singleSelection)
import Utils exposing (stopPropagationOnMousedown)
-- components
import IconMenu
import IconMenuAPI exposing (viewIcon)
import Search
import SearchAPI exposing (viewSearchInput)

import Html exposing (Html, Attribute, div, span, text, button, input, label, a)
import Html.Attributes exposing (href, style, type_, name, checked, disabled)
import Html.Events exposing (onClick)
import String exposing (fromInt)
import UndoList



-- VIEW


viewToolbar : UndoModel -> Html Msg
viewToolbar ({present} as undoModel) =
  div
    toolbarStyle
    [ viewMapNav present
    , viewSearchInput present
    , div
      []
      [ viewToolbarButton "Add Topic" AddTopic always undoModel
      , viewToolbarButton "Add Box" AddBox always undoModel
      ]
    , div
      []
      [ viewToolbarButton "Edit" (Edit EditStart) hasSelection undoModel
      , viewToolbarButton "Set Icon" (IconMenu IconMenu.Open) hasSelection undoModel
      ]
    , viewToolbarButton "Show Related" (Search Search.ShowRelated) hasSelection undoModel
    , viewMonadDisplay present
    , viewContainerDisplay present
    , viewToolbarButton "Fullscreen" (Nav Fullscreen) hasSelection undoModel
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
    backDisabled = isHome model
  in
  div
    mapNavStyle
    [ button
      [ onClick (Nav Back)
      , disabled backDisabled
      ]
      [ viewIcon "arrow-left" 20 ]
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
  case topicById (activeMap model) model of
    Just topic -> topicLabel topic
    Nothing -> "??"


viewToolbarButton : String -> Msg -> (UndoModel -> Bool) -> UndoModel -> Html Msg
viewToolbarButton label msg isEnabled undoModel =
  let
    buttonAttr =
      [ stopPropagationOnMousedown NoOp
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
    display = case singleSelection model of
      Just (topicId, mapPath) -> displayMode topicId (firstId mapPath) model.maps
      Nothing -> Nothing
    (checked1, checked2, disabled_) =
      case display of
        Just (Monad LabelOnly) -> (True, False, False)
        Just (Monad Detail) -> (False, True, False)
        _ -> (False, False, True)
  in
  div
    (displayModeStyle disabled_)
    [ div
        []
        [ text "Monad Display" ]
    , viewRadioButton "Label Only" (SwitchDisplay <| Monad LabelOnly) checked1 disabled_
    , viewRadioButton "Detail" (SwitchDisplay <| Monad Detail) checked2 disabled_
    ]


viewContainerDisplay : Model -> Html Msg
viewContainerDisplay model =
  let
    display = case singleSelection model of
      Just (topicId, mapPath) -> displayMode topicId (firstId mapPath) model.maps
      Nothing -> Nothing
    (checked1, checked2, checked3) =
      case display of
        Just (Container BlackBox) -> (True, False, False)
        Just (Container WhiteBox) -> (False, True, False)
        Just (Container Unboxed) -> (False, False, True)
        _ -> (False, False, False)
    disabled_ =
      case display of
        Just (Container _) -> False
        _ -> True
  in
  div
    (displayModeStyle disabled_)
    [ div
        []
        [ text "Container Display" ]
    , viewRadioButton "Black Box" (SwitchDisplay <| Container BlackBox) checked1 disabled_
    , viewRadioButton "White Box" (SwitchDisplay <| Container WhiteBox) checked2 disabled_
    , viewRadioButton "Unboxed" (SwitchDisplay <| Container Unboxed) checked3 disabled_
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
    [ stopPropagationOnMousedown NoOp ]
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
