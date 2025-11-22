module Tool exposing (viewAppHeader, viewTools)

import Box
import Config as C
import Item
import Model exposing (Model, Msg(..), NavMsg(..))
import ModelHelper exposing (..)
import Undo exposing (UndoModel)
import Utils as U
-- feature modules
import Icon
import IconAPI
import Search
import SearchAPI
import SelectionAPI as Sel
import TextEdit as T

import Html exposing (Html, Attribute, div, span, text, button, input, label)
import Html.Attributes exposing (style, type_, name, disabled, checked)
import Html.Events exposing (onClick)
import String exposing (fromInt)



-- GLOBAL TOOLS


viewAppHeader : UndoModel -> Html Msg
viewAppHeader ({present} as undoModel) =
  div
    appHeaderStyle
    [ viewNav present
    , div spacerStyle []
    , SearchAPI.viewInput present
    , div
      []
      [ viewButton "Add Topic" AddTopic always undoModel
      , viewButton "Add Box" AddBox always undoModel
      ]
    , div
      []
      [ viewButton "Undo" Undo Undo.hasPast undoModel
      , viewButton "Redo" Redo Undo.hasFuture undoModel
      ]
    , div
      []
      [ viewButton "Import" Import always undoModel
      , viewButton "Export" Export always undoModel
      ]
    ]


appHeaderStyle : List (Attribute Msg)
appHeaderStyle =
  [ style "display" "flex"
  , style "align-items" "center"
  , style "gap" "18px"
  , style "height" <| fromInt C.appHeaderHeight ++ "px"
  , style "padding" "0 18px 0 8px"
  , style "background-color" "lightgray"
  ]


spacerStyle : List (Attribute Msg)
spacerStyle =
  [ style "flex-grow" "1" ]


viewNav : Model -> Html Msg
viewNav model =
  let
    backDisabled = Box.isAtRoot model
  in
  div
    navStyle
    [ button
      [ onClick (Nav Back)
      , disabled backDisabled
      ]
      [ IconAPI.viewIcon "arrow-left" 20 ]
    , span
      mapTitleStyle
      [ text <| getMapName model ]
    ]


navStyle : List (Attribute Msg)
navStyle =
  []


mapTitleStyle : List (Attribute Msg)
mapTitleStyle =
  [ style "font-size" "24px"
  , style "font-weight" "bold"
  , style "margin-left" "10px"
  ]


getMapName : Model -> String
getMapName model =
  case Item.topicById (Box.active model) model of
    Just topic -> Item.topicLabel topic
    Nothing -> "??"


viewButton : String -> Msg -> (UndoModel -> Bool) -> UndoModel -> Html Msg
viewButton label msg isEnabled undoModel =
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


-- TODO: not used
{-| isEnabled predicate -}
hasSelection : UndoModel -> Bool
hasSelection undoModel =
  not (undoModel.present.selection.items |> List.isEmpty)


-- TODO: not used
{-| isEnabled predicate -}
hasBoxSelection : UndoModel -> Bool
hasBoxSelection {present} =
  case Sel.single present of
    Just (id, _) -> Item.isBox id present
    Nothing -> False


{-| isEnabled predicate -}
always : UndoModel -> Bool
always undoModel =
  True


buttonStyle : List (Attribute Msg)
buttonStyle =
  [ style "font-family" C.mainFont
  , style "font-size" <| fromInt C.toolFontSize ++ "px"
  ]



-- ITEM TOOLS


viewTools : Id -> BoxId -> Model -> Html Msg
viewTools itemId boxId model =
  div
    (toolStyle itemId boxId model)
    [ div
      []
      [ viewIconButton "Edit" (Edit T.EditStart) model
      , viewIconButton "Set Icon" (Icon Icon.OpenMenu) model
      ]
    , viewIconButton "Traverse" (Search Search.ShowRelated) model
    , viewTopicDisplay model
    , viewBoxDisplay model
    , viewIconButton "Fullscreen" (Nav Fullscreen) model
    , div
      []
      [ viewIconButton "Hide" Hide model
      , viewIconButton "Delete" Delete model
      ]
    ]


toolStyle : Id -> BoxId -> Model -> List (Attribute Msg)
toolStyle topicId boxId model =
  [ style "font-size" <| fromInt C.toolFontSize ++ "px"
  , style "position" "absolute"
  , style "top" "0"
  , style "left" "0"
  --, style "width" "100px"
  --, style "height" "22px"
  , style "background-color" "lightgray"
  ]


viewIconButton : String -> Msg -> Model -> Html Msg
viewIconButton label msg model =
  let
    buttonAttr =
      [ U.stopPropagationOnMousedown NoOp ]
  in
  button
    ( [ onClick msg ]
      ++ buttonAttr
      ++ buttonStyle
    )
    [ text label ]


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
