module IconMenuAPI exposing (closeIconMenu, updateIconMenu, viewIcon, viewIconMenu, viewTopicIcon)

-- components

import AppModel exposing (..)
import Config exposing (..)
import Dict
import FeatherIcons as Icon
import Html exposing (Attribute, Html, button, div, text)
import Html.Attributes exposing (style, title)
import Html.Events exposing (onClick)
import IconMenu exposing (IconMenuMsg(..))
import Model exposing (..)
import ModelAPI exposing (getSingleSelection, getTopicInfo, updateTopicInfo)
import Storage exposing (storeModel)
import String exposing (fromFloat)
import Utils exposing (..)



-- VIEW


viewIconMenu : Model -> List (Html Msg)
viewIconMenu model =
    if model.iconMenu.open then
        [ div
            iconMenuStyle
            [ div
                iconListStyle
                viewIconList
            , button
                ([ onClick (IconMenu Close) ]
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
    Icon.icons
        |> Dict.toList
        |> List.map
            (\( iconName, icon ) ->
                button
                    ([ onClick (Just iconName |> SetIcon |> IconMenu)
                     , stopPropagationOnMousedown NoOp
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
    case getTopicInfo topicId model of
        Just topic ->
            case topic.iconName of
                Just iconName ->
                    case Icon.icons |> Dict.get iconName of
                        Just icon ->
                            icon |> Icon.withSize topicIconSize |> Icon.toHtml topicIconStyle

                        Nothing ->
                            text "??"

                Nothing ->
                    text ""

        Nothing ->
            text "?"


viewIcon : String -> Float -> Html Msg
viewIcon iconName size =
    case Icon.icons |> Dict.get iconName of
        Just icon ->
            icon |> Icon.withSize size |> Icon.toHtml []

        Nothing ->
            text "??"


topicIconStyle : List (Attribute Msg)
topicIconStyle =
    [ style "position" "relative"
    , style "top" <| fromFloat ((topicSize.h - topicIconSize) / 2) ++ "px"
    , style "left" <| fromFloat ((topicSize.h - topicIconSize) / 2) ++ "px"
    , style "color" "white"
    ]



-- UPDATE


updateIconMenu : IconMenuMsg -> Model -> ( Model, Cmd Msg )
updateIconMenu msg model =
    case msg of
        Open ->
            ( openIconMenu model, Cmd.none )

        Close ->
            ( closeIconMenu model, Cmd.none )

        SetIcon maybeIcon ->
            setIcon maybeIcon model
                |> closeIconMenu
                |> storeModel


openIconMenu : Model -> Model
openIconMenu ({ iconMenu } as model) =
    { model | iconMenu = { iconMenu | open = True } }


closeIconMenu : Model -> Model
closeIconMenu ({ iconMenu } as model) =
    { model | iconMenu = { iconMenu | open = False } }


setIcon : Maybe IconName -> Model -> Model
setIcon iconName model =
    case getSingleSelection model of
        Just ( id, _ ) ->
            updateTopicInfo id
                (\topic -> { topic | iconName = iconName })
                model

        Nothing ->
            model



-- FIXME: illegal state -> make Edit dialog modal
