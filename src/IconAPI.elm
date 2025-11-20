module IconAPI exposing (viewIcon, viewTopicIcon, viewMenu, closeMenu, update)

import Config as C
import Item
import Model exposing (Model, Msg(..))
import ModelHelper exposing (..)
import Storage as S
import Undo exposing (UndoModel)
import Utils as U
-- feature modules
import Icon
import SelectionAPI as Sel

import Dict
import Html exposing (Html, Attribute, div, text, button)
import Html.Attributes exposing (title, style)
import Html.Events exposing (onClick)
import String exposing (fromFloat)
import FeatherIcons as FI



-- VIEW


viewMenu : Model -> List (Html Msg)
viewMenu model =
  case model.icon.menu of
    Icon.Open ->
      [ div
        iconMenuStyle
        [ div
          iconListStyle
          viewIconList
        , button
          ( [onClick (Icon Icon.CloseMenu)]
            ++ closeButtonStyle
          )
          [ FI.x
            |> FI.withSize 12
            |> FI.toHtml []
          ]
        ]
      ]
    Icon.Closed -> []


iconMenuStyle : List (Attribute Msg)
iconMenuStyle =
  [ style "position" "absolute"
  , style "top" "291px"
  , style "width" "320px"
  , style "height" "320px"
  , style "background-color" "white"
  , style "border" "1px solid lightgray"
  , style "z-index" "1"
  ]


iconListStyle : List (Attribute Msg)
iconListStyle =
  [ style "height" "100%"
  , style "overflow" "auto"
  ]


closeButtonStyle : List (Attribute Msg)
closeButtonStyle =
  [ style "position" "absolute"
  , style "top" "0"
  , style "right" "0"
  ]


viewIconList : List (Html Msg)
viewIconList =
  FI.icons |> Dict.toList |> List.map
    (\(iconName, icon) ->
      button
        ( [ onClick (Just iconName |> Icon.SetIcon |> Icon)
          , U.stopPropagationOnMousedown NoOp
          , title iconName
          ]
          ++ iconButtonStyle
        )
        [ FI.toHtml [] icon ]
    )


iconButtonStyle : List (Attribute Msg)
iconButtonStyle =
  [ style "border-width" "0"
  , style "margin" "8px"
  ]


viewTopicIcon : Id -> Model -> Html Msg
viewTopicIcon topicId model =
  case Item.topicById topicId model of
    Just topic ->
      case topic.icon of
        Just iconName ->
          case FI.icons |> Dict.get iconName of
            Just icon -> icon |> FI.withSize C.topicIconSize |> FI.toHtml topicIconStyle
            Nothing -> text "??"
        Nothing -> text ""
    Nothing -> text "?"


viewIcon : String -> Float -> Html Msg
viewIcon iconName size =
  case FI.icons |> Dict.get iconName of
    Just icon -> icon |> FI.withSize size |> FI.toHtml []
    Nothing -> text "??"


topicIconStyle : List (Attribute Msg)
topicIconStyle =
  [ style "position" "relative"
  , style "top" <| fromFloat ((C.topicSize.h - C.topicIconSize) / 2) ++ "px"
  , style "left" <| fromFloat ((C.topicSize.h - C.topicIconSize) / 2) ++ "px"
  , style "color" "white"
  ]



-- UPDATE


update : Icon.Msg -> UndoModel -> (UndoModel, Cmd Msg)
update msg ({present} as undoModel) =
  case msg of
    Icon.OpenMenu -> (openMenu present, Cmd.none) |> Undo.swap undoModel
    Icon.CloseMenu -> (closeMenu present, Cmd.none) |> Undo.swap undoModel
    Icon.SetIcon maybeIcon -> setIcon maybeIcon present
      |> closeMenu
      |> S.store
      |> Undo.push undoModel


openMenu : Model -> Model
openMenu ({icon} as model) =
  { model | icon = { icon | menu = Icon.Open }}


closeMenu : Model -> Model
closeMenu ({icon} as model) =
  { model | icon = { icon | menu = Icon.Closed }}


setIcon : Maybe Icon -> Model -> Model
setIcon iconName model =
  case Sel.single model of
    Just (id, _) -> Item.updateTopicInfo id
      (\topic -> { topic | icon = iconName })
      model
    Nothing -> model -- FIXME: illegal state -> make Edit dialog modal?
