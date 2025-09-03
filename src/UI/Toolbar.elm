module UI.Toolbar exposing (viewToolbar)

import AppModel as AM exposing (Msg(..))
import Feature.OpenDoor.Decide exposing (decideOpenDoorMsg)
import Html exposing (Html, button, div, text)
import Html.Attributes as Attr
import Html.Events exposing (onClick)
import Toolbar
import Utils exposing (stopPropagationOnMousedown)


viewToolbar : AM.Model -> Html AM.Msg
viewToolbar model =
    div []
        [ crossButton model
        , Toolbar.viewToolbar model
        ]


crossButton : AM.Model -> Html AM.Msg
crossButton model =
    let
        ( disabled_, msg ) =
            case decideOpenDoorMsg model of
                Just m ->
                    ( False, m )

                Nothing ->
                    ( True, NoOp )
    in
    button
        [ Attr.id "btn-Cross"
        , Attr.disabled disabled_
        , stopPropagationOnMousedown NoOp
        , onClick msg
        ]
        [ text "Cross" ]
