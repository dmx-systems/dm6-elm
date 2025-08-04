module IconMenu exposing (viewIconMenu, viewTopicIcon, updateIconMenu)

import Config exposing (..)
import Model exposing (..)
import Storage exposing (storeModel)
import Utils exposing (..)

import Dict
import Html exposing (Html, Attribute, div, text, button)
import Html.Attributes exposing (title, style)
import Html.Events exposing (onClick)
import String exposing (fromFloat)
import FeatherIcons as Icon



-- UPDATE


updateIconMenu : IconMenuMsg -> Model -> (Model, Cmd Msg)
updateIconMenu msg model =
  case msg of
    Open -> (setIconMenuState True model, Cmd.none)
    Close -> (setIconMenuState False model, Cmd.none)
    SetIcon maybeIcon -> setIcon maybeIcon model
      |> setIconMenuState False
      |> storeModel


setIconMenuState : Bool -> Model -> Model
setIconMenuState isOpen model =
  { model | iconMenuState = isOpen }


setIcon : Maybe IconName -> Model -> Model
setIcon iconName model =
  case getSingleSelection model of
    Just (id, _) -> updateTopicInfo id
      (\topic -> { topic | iconName = iconName })
      model
    Nothing -> model -- FIXME: illegal state -> make Edit dialog modal



-- VIEW


viewIconMenu : Model -> Html Msg
viewIconMenu model =
  div
    iconMenuStyle
    [ div
        iconListStyle
        viewIconList
    , button
      ( [onClick (IconMenu Close)]
        ++ closeButtonStyle
      )
      [ Icon.x
        |> Icon.withSize 12
        |> Icon.toHtml []
      ]
    ]


viewIconList : List (Html Msg)
viewIconList =
  Icon.icons |> Dict.toList |> List.map
    (\(iconName, icon) ->
      button
        ( [ onClick (Just iconName |> SetIcon |> IconMenu)
          , stopPropagationOnMousedown NoOp
          , title iconName
          ]
          ++ iconButtonStyle
        )
        [ Icon.toHtml [] icon ]
    )


viewTopicIcon : Id -> Model -> Html Msg
viewTopicIcon topicId model =
  case getTopicInfo topicId model of
    Just topic ->
      case topic.iconName of
        Just iconName ->
          case Icon.icons |> Dict.get iconName of
            Just icon -> icon |> Icon.withSize topicIconSize |> Icon.toHtml topicIconStyle
            Nothing -> text "??"
        Nothing -> text ""
    Nothing -> text "?"



-- STYLE


iconMenuStyle : List (Attribute Msg)
iconMenuStyle =
  [ style "position" "absolute"
  , style "left" "72px"
  , style "top" "244px"
  , style "width" "320px"
  , style "height" "320px"
  , style "background-color" "white"
  , style "border" "1px solid lightgray"
  ]


iconListStyle : List (Attribute Msg)
iconListStyle =
  [ style "height" "100%"
  , style "overflow" "auto"
  ]


iconButtonStyle : List (Attribute Msg)
iconButtonStyle =
  [ style "border-width" "0"
  , style "margin" "8px"
  ]


closeButtonStyle : List (Attribute Msg)
closeButtonStyle =
  [ style "position" "absolute"
  , style "top" "0"
  , style "right" "0"
  ]


topicIconStyle : List (Attribute Msg)
topicIconStyle =
  [ style "position" "relative"
  , style "top" <| fromFloat ((topicSize.h - topicIconSize) / 2) ++ "px"
  , style "left" <| fromFloat ((topicSize.h - topicIconSize) / 2) ++ "px"
  , style "color" "white"
  ]
