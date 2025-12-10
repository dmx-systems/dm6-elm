module Feature.IconAPI exposing (view, viewTopicIcon, viewMenu, closeMenu, update)

import Feature.Icon as Icon
import Feature.SelAPI as SelAPI
import Item
import Model exposing (Model, Msg(..))
import ModelParts exposing (..)
import Storage as S
import Undo exposing (UndoModel)
import Utils as U

import Dict
import Html exposing (Html, Attribute, div, text, button)
import Html.Attributes exposing (class, title, style)
import Html.Events exposing (onClick)
import FeatherIcons as FI



-- VIEW


viewMenu : Model -> List (Html Msg)
viewMenu model =
  case model.icon.menu of
    Icon.Open ->
      [ div
        iconMenuStyle
        viewIconList
      ]
    Icon.Closed -> []


iconMenuStyle : List (Attribute Msg)
iconMenuStyle =
  [ style "position" "absolute"
  , style "left" "35px"
  , style "width" "288px"
  , style "white-space" "initial" -- don't inherit from item toolbar
  , style "background-color" "white"
  , style "border" "1px solid lightgray"
  ]


viewIconList : List (Html Msg)
viewIconList =
  FI.icons |> Dict.toList |> List.map
    (\(iconName, icon) ->
      button
        ( [ class "tool"
          , title iconName
          , onClick (Just iconName |> Icon.SetIcon |> Icon)
          , U.stopPropagationOnMousedown NoOp
          ]
          ++ iconButtonStyle
        )
        [ FI.toHtml [] icon ]
    )


iconButtonStyle : List (Attribute Msg)
iconButtonStyle =
  [ style "border" "none"
  , style "margin" "8px"
  , style "background-color" "transparent"
  ]


viewTopicIcon : Id -> Float -> List (Attribute Msg) -> Model -> Html Msg
viewTopicIcon topicId size style_ model =
  case Item.topicById topicId model of
    Just topic ->
      case topic.icon of
        Just iconName ->
          view iconName size style_
        Nothing -> text ""
    Nothing -> text "?"


view : String -> Float -> List (Attribute Msg) -> Html Msg
view iconName size style_ =
  case FI.icons |> Dict.get iconName of
    Just icon ->
      icon
      |> FI.withSize size
      |> FI.toHtml style_
    Nothing -> text "??"



-- UPDATE


update : Icon.Msg -> UndoModel -> (UndoModel, Cmd Msg)
update msg ({present} as undoModel) =
  case msg of
    Icon.OpenMenu -> (openMenu present, Cmd.none) |> Undo.swap undoModel
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
  case SelAPI.single model of
    Just (id, _) -> Item.updateTopic id
      (\topic -> { topic | icon = iconName })
      model
    Nothing -> model
