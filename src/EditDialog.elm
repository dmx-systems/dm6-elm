module EditDialog exposing (viewEditDialog)

import Model exposing (..)
import Style exposing (..)

import Html exposing (Html, div, text, button, input)
import Html.Events exposing (onClick)
import FeatherIcons as Icon



viewEditDialog : Model -> Html Msg
viewEditDialog model =
  div
    editDialogStyle
    [ Icon.x |> Icon.toHtml
      ( closeButtonStyle
        ++ [onClick (Edit Close)]
      )
    ]
