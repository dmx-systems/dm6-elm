module IconMenuAPI exposing (viewIcon, viewTopicIcon, view, close, update)

import AppModel exposing (..)
import Config as C
import Model exposing (..)
import ModelAPI as A
import Storage as S
import Utils as U
-- feature modules
import IconMenu

import Dict
import Html exposing (Html, Attribute, div, text, button)
import Html.Attributes exposing (title, style)
import Html.Events exposing (onClick)
import String exposing (fromFloat)
import FeatherIcons as Icon



-- VIEW


view : Model -> List (Html Msg)
view model =
  if model.iconMenu.open then
    [ div
      iconMenuStyle
      [ div
        iconListStyle
        viewIconList
      , button
        ( [onClick (IconMenu IconMenu.Close)]
          ++ closeButtonStyle
        )
        [ Icon.x
          |> Icon.withSize 12
          |> Icon.toHtml []
        ]
      ]
    ]
  else
    []


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
  Icon.icons |> Dict.toList |> List.map
    (\(iconName, icon) ->
      button
        ( [ onClick (Just iconName |> IconMenu.SetIcon |> IconMenu)
          , U.stopPropagationOnMousedown NoOp
          , title iconName
          ]
          ++ iconButtonStyle
        )
        [ Icon.toHtml [] icon ]
    )


iconButtonStyle : List (Attribute Msg)
iconButtonStyle =
  [ style "border-width" "0"
  , style "margin" "8px"
  ]


viewTopicIcon : Id -> Model -> Html Msg
viewTopicIcon topicId model =
  case A.topicById topicId model of
    Just topic ->
      case topic.iconName of
        Just iconName ->
          case Icon.icons |> Dict.get iconName of
            Just icon -> icon |> Icon.withSize C.topicIconSize |> Icon.toHtml topicIconStyle
            Nothing -> text "??"
        Nothing -> text ""
    Nothing -> text "?"


viewIcon : String -> Float -> Html Msg
viewIcon iconName size =
  case Icon.icons |> Dict.get iconName of
    Just icon -> icon |> Icon.withSize size |> Icon.toHtml []
    Nothing -> text "??"


topicIconStyle : List (Attribute Msg)
topicIconStyle =
  [ style "position" "relative"
  , style "top" <| fromFloat ((C.topicSize.h - C.topicIconSize) / 2) ++ "px"
  , style "left" <| fromFloat ((C.topicSize.h - C.topicIconSize) / 2) ++ "px"
  , style "color" "white"
  ]



-- UPDATE


update : IconMenu.Msg -> UndoModel -> (UndoModel, Cmd Msg)
update msg ({present} as undoModel) =
  case msg of
    IconMenu.Open -> (openIconMenu present, Cmd.none) |> A.swap undoModel
    IconMenu.Close -> (close present, Cmd.none) |> A.swap undoModel
    IconMenu.SetIcon maybeIcon -> setIcon maybeIcon present
      |> close
      |> S.store
      |> A.push undoModel


openIconMenu : Model -> Model
openIconMenu ({iconMenu} as model) =
  { model | iconMenu = { iconMenu | open = True }}


close : Model -> Model
close ({iconMenu} as model) =
  { model | iconMenu = { iconMenu | open = False }}


setIcon : Maybe IconName -> Model -> Model
setIcon iconName model =
  case A.singleSelection model of
    Just (id, _) -> A.updateTopicInfo id
      (\topic -> { topic | iconName = iconName })
      model
    Nothing -> model -- FIXME: illegal state -> make Edit dialog modal?
