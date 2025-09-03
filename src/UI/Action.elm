module UI.Action exposing
    ( Action
    , action
    , viewButton
    , viewToolbar
    , withEnabled
    )

{-| Minimal, reusable “Action” concept + simple views.

    import UI.Action as Act

    myActions : List (Act.Action Msg)
    myActions =
        [ action "add" "Add Topic" (Just "plus") True AddTopic
        , action "edit" "Edit" (Just "edit") model.hasSelection EditPressed
        , action "cross" "Cross" (Just "link") (hasSelection model) CrossPressed
        ]

    view model =
        div []
            [ Act.viewToolbar myActions
            , ...
            ]

-}

import Html exposing (Html, button, div, i, text)
import Html.Attributes as Attr
import Html.Events exposing (onClick)


{-| First-class action description.
-}
type alias Action msg =
    { id : String
    , label : String
    , icon : Maybe String
    , enabled : Bool
    , onTrigger : msg
    }


{-| Convenience constructor.
-}
action : String -> String -> Maybe String -> Bool -> msg -> Action msg
action id label icon enabled onTrigger =
    { id = id
    , label = label
    , icon = icon
    , enabled = enabled
    , onTrigger = onTrigger
    }


{-| Toggle enabled flag (handy in pipelines).
-}
withEnabled : Bool -> Action msg -> Action msg
withEnabled isEnabled act =
    { act | enabled = isEnabled }


{-| Render a single action as a button.
-}
viewButton : Action msg -> Html msg
viewButton act =
    let
        iconView =
            case act.icon of
                Just name ->
                    -- Assumes an icon font or CSS class like `.icon-link`.
                    i [ Attr.class ("icon-" ++ name), Attr.style "margin-right" "0.4rem" ] []

                Nothing ->
                    text ""
    in
    button
        [ Attr.class "ui-action"
        , Attr.attribute "data-action-id" act.id
        , Attr.disabled (not act.enabled)
        , Attr.title act.id
        , onClick act.onTrigger
        ]
        [ iconView, text act.label ]


{-| Render a horizontal toolbar of actions.
-}
viewToolbar : List (Action msg) -> Html msg
viewToolbar actions =
    div [ Attr.class "ui-toolbar" ]
        (List.map viewButton actions)
