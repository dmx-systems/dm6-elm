module EditDialog exposing (viewEditDialog, viewTopicIcon)

import Model exposing (..)
import Style exposing (..)

import Dict
import Html exposing (Html, div, text, button)
import Html.Attributes exposing (title)
import Html.Events exposing (onClick)
import FeatherIcons as Icon



viewEditDialog : Model -> Html Msg
viewEditDialog model =
  div
    editDialogStyle
    [ div
        iconsStyle
        viewIcons
    , button
      ( [onClick (Edit Close)]
        ++ closeButtonStyle
      )
      [ Icon.x
        |> Icon.withSize 12
        |> Icon.toHtml []
      ]
    ]


viewIcons : List (Html Msg)
viewIcons =
  Icon.icons |> Dict.toList |> List.map
    (\(iconName, icon) ->
      button
        ( [ onClick (Just iconName |> SetIcon |> Edit)
          , stopPropagationOnMousedown
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
            Just icon -> icon |> Icon.toHtml topicIconStyle
            Nothing -> text "??"
        Nothing -> text ""
    Nothing -> text "?"
