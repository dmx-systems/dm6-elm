module Feature.Cross exposing (Msg(..), view)

import Html exposing (button, text, Html)
import Html.Attributes as Attr
import Html.Events exposing (onClick)

-- Local message type for this feature
type Msg
    = CrossClick

-- The button view
view : Html Msg
view =
    button
        [ Attr.id "btn-Cross"
        , onClick CrossClick
        ]
        [ text "Cross" ]
