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
import TextEdit as T

import Html exposing (Html, Attribute, div, span, text, button)
import Html.Attributes exposing (style, title, disabled)
import Html.Events exposing (onClick)
import String exposing (fromInt)



-- GLOBAL TOOLS


viewAppHeader : UndoModel -> Html Msg
viewAppHeader ({present} as undoModel) =
  div
    appHeaderStyle
    [ viewNav present
    , div spacerStyle []
    , div
      []
      [ viewButton "Add Topic" AddTopic always undoModel
      , viewButton "Add Box" AddBox always undoModel
      ]
    , SearchAPI.viewInput present
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
  , style "background-color" C.toolbarColor
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
  button
    ( [ onClick msg
      , disabled <| not <| isEnabled undoModel
      , U.stopPropagationOnMousedown NoOp
      ]
      ++ buttonStyle
    )
    [ text label ]


buttonStyle : List (Attribute Msg)
buttonStyle =
  [ style "font-family" C.mainFont
  , style "font-size" <| fromInt C.toolFontSize ++ "px"
  ]


{-| isEnabled predicate -}
always : UndoModel -> Bool
always undoModel =
  True



-- ITEM TOOLS


viewTools : Id -> BoxId -> Model -> List (Html Msg)
viewTools itemId boxId model =
  [ viewToolbar itemId boxId model
  , viewCaret itemId boxId model
  ]


viewToolbar : Id -> BoxId -> Model -> Html Msg
viewToolbar itemId boxId model =
  let
    topicTools =
      [ viewIconButton "Edit" "edit-3" False (Edit T.EditStart)
      , viewIconButton "Set Icon" "image" False (Icon Icon.OpenMenu)
      , viewIconButton "Traverse" "share-2" False (Search Search.ShowRelated)
      , viewIconButton "Delete" "trash" False Delete
      , viewIconButton "Remove" "x" False Hide -- TODO: "hide" -> "remove"
      ]
    boxTools =
      if Item.isBox itemId model then
        [ viewSpacer
        , viewIconButton "Unbox" "external-link" isUnboxed (Unbox itemId boxId)
        , viewIconButton "Fullscreen" "maximize-2" False (Nav Fullscreen)
        ]
      else
        []
    isUnboxed =
      case Box.displayMode itemId boxId model of
        Just (BoxD Unboxed) -> True
        _ -> False
  in
  div
    (toolbarStyle itemId boxId model)
    (topicTools ++ boxTools)


toolbarStyle : Id -> BoxId -> Model -> List (Attribute Msg)
toolbarStyle topicId boxId model =
  [ style "position" "absolute"
  , style "top" "-32px"
  , style "white-space" "nowrap"
  , style "background-color" C.toolbarColor
  ]


viewSpacer : Html Msg
viewSpacer =
  span
    [ style "display" "inline-block"
    , style "width" "12px"
    ]
    []


viewCaret : Id -> BoxId -> Model -> Html Msg
viewCaret itemId boxId model =
  let
    icon =
      case Box.displayMode itemId boxId model of
        Just (TopicD LabelOnly) -> "chevron-right"
        Just (TopicD Detail) -> "chevron-down"
        Just (BoxD BlackBox) -> "chevron-right"
        Just (BoxD WhiteBox) -> "chevron-down"
        Just (BoxD Unboxed) -> "chevron-down"
        Nothing -> "??"
  in
  button
    ( [ onClick <| ToggleDisplay itemId boxId
      , U.stopPropagationOnMousedown NoOp
      ]
      ++ caretStyle
    )
    [ IconAPI.viewIcon icon 20 ]


caretStyle : List (Attribute Msg)
caretStyle  =
  [ style "position" "absolute"
  , style "top" "-1px"
  , style "left" "-35px"
  ]


viewIconButton : String -> String -> Bool -> Msg -> Html Msg
viewIconButton label icon disabled_ msg =
  button
    [ onClick msg
    , disabled disabled_
    , title label
    , U.stopPropagationOnMousedown NoOp
    ]
    [ IconAPI.viewIcon icon 20 ]
