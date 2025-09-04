module Toolbar exposing (viewToolbar)

import AppModel exposing (Model, Msg(..))
import Config exposing (date, footerFontSize, homeMapName, mainFont, toolbarFontSize, version)
import Html exposing (Attribute, Html, a, button, div, input, label, span, text)
import Html.Attributes exposing (checked, disabled, href, name, style, type_)
import Html.Events exposing (onClick)
import IconMenu exposing (IconMenuMsg(..))
import IconMenuAPI exposing (viewIcon)
import Model
    exposing
        ( ContainerDisplay(..)
        , DisplayMode(..)
        , EditMsg(..)
        , MonadDisplay(..)
        , NavMsg(..)
        )
import ModelAPI
    exposing
        ( activeMap
        , getDisplayMode
        , getMapId
        , getSingleSelection
        , getTopicInfo
        , getTopicLabel
        , isHome
        )
import SearchAPI exposing (viewSearchInput)
import String exposing (fromInt)
import Utils exposing (stopPropagationOnMousedown)



-- VIEW


viewToolbar : Model -> Html Msg
viewToolbar model =
    div
        toolbarStyle
        [ viewMapNav model
        , viewSearchInput model
        , viewToolbarButton "Add Topic" AddTopic False model
        , viewToolbarButton "Edit" (Edit EditStart) True model
        , viewToolbarButton "Choose Icon" (IconMenu Open) True model
        , viewMonadDisplay model
        , viewContainerDisplay model
        , viewToolbarButton "Hide" Hide True model
        , viewToolbarButton "Fullscreen" (Nav Fullscreen) True model
        , viewToolbarButton "Delete" Delete True model
        , viewFooter
        ]


toolbarStyle : List (Attribute Msg)
toolbarStyle =
    [ style "font-size" <| fromInt toolbarFontSize ++ "px"
    , style "display" "flex"
    , style "flex-direction" "column"
    , style "align-items" "flex-start"
    , style "gap" "28px"
    , style "position" "fixed"
    , style "z-index" "1"
    ]


viewMapNav : Model -> Html Msg
viewMapNav model =
    let
        backDisabled =
            isHome model
    in
    div
        mapNavStyle
        [ button
            [ onClick (Nav Back)
            , disabled backDisabled
            ]
            [ viewIcon "arrow-left" 20 ]
        , span
            mapTitleStyle
            [ text <| getMapName model ]
        ]


mapNavStyle : List (Attribute Msg)
mapNavStyle =
    [ style "margin-top" "20px"
    , style "margin-bottom" "12px"
    ]


mapTitleStyle : List (Attribute Msg)
mapTitleStyle =
    [ style "font-size" "36px"
    , style "font-weight" "bold"
    , style "vertical-align" "top"
    , style "margin-left" "12px"
    ]


getMapName : Model -> String
getMapName model =
    if isHome model then
        -- home map has no corresponding topic
        homeMapName

    else
        case getTopicInfo (activeMap model) model of
            Just topic ->
                getTopicLabel topic

            Nothing ->
                "??"


viewToolbarButton : String -> Msg -> Bool -> Model -> Html Msg
viewToolbarButton label msg requireSelection model =
    let
        hasNoSelection =
            List.isEmpty model.selection

        buttonAttr =
            if requireSelection then
                [ stopPropagationOnMousedown NoOp
                , disabled hasNoSelection
                ]

            else
                []
    in
    button
        (onClick msg :: buttonAttr ++ buttonStyle)
        [ text label ]


buttonStyle : List (Attribute Msg)
buttonStyle =
    [ style "font-family" mainFont
    , style "font-size" <| fromInt toolbarFontSize ++ "px"
    ]


viewMonadDisplay : Model -> Html Msg
viewMonadDisplay model =
    let
        displayMode =
            Maybe.andThen
                (\( topicId, mapPath ) -> getDisplayMode topicId (getMapId mapPath) model.maps)
                (getSingleSelection model)

        ( checked1, checked2, disabled_ ) =
            case displayMode of
                Just (Monad LabelOnly) ->
                    ( True, False, False )

                Just (Monad Detail) ->
                    ( False, True, False )

                _ ->
                    ( False, False, True )
    in
    div
        (displayModeStyle disabled_)
        [ div
            []
            [ text "Monad Display" ]
        , viewRadioButton "Label Only" (SwitchDisplay <| Monad LabelOnly) checked1 disabled_
        , viewRadioButton "Detail" (SwitchDisplay <| Monad Detail) checked2 disabled_
        ]


viewContainerDisplay : Model -> Html Msg
viewContainerDisplay model =
    let
        displayMode =
            Maybe.andThen
                (\( topicId, mapPath ) -> getDisplayMode topicId (getMapId mapPath) model.maps)
                (getSingleSelection model)

        ( checked1, checked2, checked3 ) =
            case displayMode of
                Just (Container BlackBox) ->
                    ( True, False, False )

                Just (Container WhiteBox) ->
                    ( False, True, False )

                Just (Container Unboxed) ->
                    ( False, False, True )

                _ ->
                    ( False, False, False )

        disabled_ =
            case displayMode of
                Just (Container _) ->
                    False

                _ ->
                    True
    in
    div
        (displayModeStyle disabled_)
        [ div
            []
            [ text "Container Display" ]
        , viewRadioButton "Black Box" (SwitchDisplay <| Container BlackBox) checked1 disabled_
        , viewRadioButton "White Box" (SwitchDisplay <| Container WhiteBox) checked2 disabled_
        , viewRadioButton "Unboxed" (SwitchDisplay <| Container Unboxed) checked3 disabled_
        ]


displayModeStyle : Bool -> List (Attribute Msg)
displayModeStyle disabled =
    let
        ( color, pointerEvents ) =
            if disabled then
                ( "gray", "none" )

            else
                ( "unset", "unset" )
    in
    [ style "display" "flex"
    , style "flex-direction" "column"
    , style "gap" "6px"
    , style "color" color
    , style "pointer-events" pointerEvents
    ]


viewRadioButton : String -> Msg -> Bool -> Bool -> Html Msg
viewRadioButton label_ msg isChecked isDisabled =
    label
        [ stopPropagationOnMousedown NoOp ]
        [ input
            [ type_ "radio"
            , name "display-mode"
            , checked isChecked
            , disabled isDisabled
            , onClick msg
            ]
            []
        , text label_
        ]


viewFooter : Html Msg
viewFooter =
    div
        footerStyle
        [ div
            []
            [ text version ]
        , div
            []
            [ text date ]
        , div
            []
            [ text "Source: "
            , a
                (href "https://github.com/dmx-systems/dm6-elm" :: linkStyle)
                [ text "GitHub" ]
            ]
        , a
            (href "https://dmx.berlin" :: linkStyle)
            [ text "DMX Systems" ]
        ]


footerStyle : List (Attribute Msg)
footerStyle =
    [ style "font-size" <| fromInt footerFontSize ++ "px"
    , style "color" "lightgray"
    ]


linkStyle : List (Attribute Msg)
linkStyle =
    [ style "color" "lightgray" ]
