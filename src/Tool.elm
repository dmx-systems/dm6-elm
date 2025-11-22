module Tool exposing (viewAppHeader, viewButton, hasSelection, hasBoxSelection)

import Box
import Config as C
import Item
import Model exposing (Model, Msg(..), NavMsg(..))
import Undo exposing (UndoModel)
import Utils as U
-- feature modules
import IconAPI
import SearchAPI
import SelectionAPI as Sel

import Html exposing (Html, Attribute, div, span, text, button)
import Html.Attributes exposing (style, disabled)
import Html.Events exposing (onClick)
import String exposing (fromInt)



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
  [ style "font-size" "32px"
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


{-| isEnabled predicate -}
hasSelection : UndoModel -> Bool
hasSelection undoModel =
  not (undoModel.present.selection.items |> List.isEmpty)


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
