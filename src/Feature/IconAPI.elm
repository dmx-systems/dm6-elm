module Feature.IconAPI exposing (view, viewTopicIcon, viewPicker, openPicker, closePicker,
  update)

import Feature.Icon as Icon
import Feature.SelAPI as SelAPI
import Item
import Model exposing (Model, Msg(..))
import ModelParts exposing (..)
import Storage as S
import Undo exposing (UndoModel)
import Utils as U

import Dict
import Html exposing (Html, div, text, button)
import Html.Attributes exposing (class, title, style)
import Html.Events exposing (onClick)
import FeatherIcons as FI



-- VIEW


viewPicker : Model -> List (Html Msg)
viewPicker model =
  case model.icon.picker of
    Icon.Open ->
      [ div
        pickerStyle
        viewIconList
      ]
    Icon.Closed -> []


pickerStyle : Attributes Msg
pickerStyle =
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
          , onClick (Just iconName |> Icon.IconSelected |> Icon)
          , U.onMouseDownStop NoOp -- Prevent icon picker closing
          ]
          ++ iconButtonStyle
        )
        [ FI.toHtml [] icon ]
    )


iconButtonStyle : Attributes Msg
iconButtonStyle =
  [ style "border" "none"
  , style "margin" "8px"
  , style "background-color" "transparent"
  ]


viewTopicIcon : Id -> Int -> Attributes Msg -> Model -> Html Msg
viewTopicIcon topicId size style_ model =
  case Item.topicById topicId model of
    Just topic ->
      case topic.icon of
        Just iconName ->
          view iconName size style_
        Nothing -> text ""
    Nothing -> text "?"


view : String -> Int -> Attributes Msg -> Html Msg
view iconName size style_ =
  case FI.icons |> Dict.get iconName of
    Just icon ->
      icon
      |> FI.withSize (toFloat size)
      |> FI.toHtml style_
    Nothing -> text "??"



-- UPDATE


update : Icon.Msg -> UndoModel -> (UndoModel, Cmd Msg)
update msg ({present} as undoModel) =
  case msg of
    Icon.IconSelected maybeIcon -> setIcon maybeIcon present
      |> closePicker
      |> S.store
      |> Undo.push undoModel


openPicker : Model -> Model
openPicker ({icon} as model) =
  { model | icon = { icon | picker = Icon.Open }}


closePicker : Model -> Model
closePicker ({icon} as model) =
  { model | icon = { icon | picker = Icon.Closed }}


setIcon : Maybe Icon -> Model -> Model
setIcon iconName model =
  case SelAPI.single model of
    Just (id, _) -> Item.updateTopic id
      (\topic -> { topic | icon = iconName })
      model
    Nothing -> model
