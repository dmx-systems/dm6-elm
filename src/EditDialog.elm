module EditDialog exposing (viewEditDialog)

import Model exposing (..)
import Style exposing (..)

import Dict
import Html exposing (Html, div, text, button)
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
    (\(name, icon) ->
      button
        iconButtonStyle
        [ Icon.toHtml [] icon ]
    )
