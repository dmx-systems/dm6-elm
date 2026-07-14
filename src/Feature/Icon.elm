module Feature.Icon exposing (view, viewTopicIcon, viewPicker, openPicker, closePicker, update)

import Env exposing (Env2)
import Feature.IconDef as IconDef
import Feature.Sel as Sel
import Model exposing (Model, Msg(..))
import ModelBase exposing (..)
import Outcome exposing (Outcome, Directives, Storage(..), History(..))
import Shared.Events as Events
import Topic

import Dict
import Html exposing (Html, div, text, button)
import Html.Attributes exposing (class, title, style)
import Html.Events exposing (onClick)
import FeatherIcons as FI



-- VIEW


viewPicker : Model -> List (Html Msg)
viewPicker model =
  case model.icon.picker of
    IconDef.Open ->
      [ div
        pickerStyle
        viewIconList
      ]
    IconDef.Closed ->
      []


pickerStyle : Attrs Msg
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
          , onClick (Just iconName |> IconDef.IconSelected |> Icon)
          , Events.onPointerDownStop NoOp -- Prevent icon picker closing
          ]
          ++ iconButtonStyle
        )
        [ FI.toHtml [] icon ]
    )


iconButtonStyle : Attrs Msg
iconButtonStyle =
  [ style "border" "none"
  , style "margin" "8px"
  , style "background-color" "transparent"
  ]


viewTopicIcon : TopicId -> Int -> Attrs Msg -> Model -> Html Msg
viewTopicIcon topicId size style_ model =
  case Topic.fromId topicId model of
    Just {icon} ->
      case icon of
        Just iconName ->
          view iconName size style_
        Nothing ->
          text ""
    Nothing ->
      text "?"


view : String -> Int -> Attrs Msg -> Html Msg
view iconName size style_ =
  case FI.icons |> Dict.get iconName of
    Just icon ->
      icon
        |> FI.withSize (toFloat size)
        |> FI.toHtml style_
    Nothing ->
      text "??"



-- UPDATE


update : IconDef.Msg -> Env2 -> Outcome
update msg env =
  case msg of
    IconDef.IconSelected icon ->
      env
        |> Env.map (setIcon icon)
        |> Env.map closePicker
        |> Env.outcomeWith (Directives Store Push)


setIcon : Maybe Icon -> Model -> Model
setIcon iconName model =
  case Sel.single model of
    Just (T topicId, _) ->
      model
        |> Topic.update topicId
          (\topic -> { topic | icon = iconName })
    _ ->
      model


openPicker : Model -> Model
openPicker ({icon} as model) =
  { model | icon = { icon | picker = IconDef.Open }}


closePicker : Model -> Model
closePicker ({icon} as model) =
  { model | icon = { icon | picker = IconDef.Closed }}
