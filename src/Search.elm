module Search exposing (closeResultMenu, updateSearch, viewResultMenu, viewSearchInput)

import Config exposing (..)
import Dict
import Html exposing (Attribute, Html, div, input, text)
import Html.Attributes exposing (attribute, style, value)
import Html.Events exposing (on, onFocus, onInput)
import Json.Decode as D
import Model exposing (..)
import Storage exposing (storeModel)
import String exposing (fromInt)
import Utils exposing (..)



-- VIEW


viewSearchInput : Model -> Html Msg
viewSearchInput model =
    div
        []
        [ div
            []
            [ text "Search" ]
        , input
            ([ value model.searchText
             , onInput (Search << SearchInput)
             , onFocus (Search SearchFocus)
             ]
                ++ searchInputStyle
            )
            []
        ]


searchInputStyle : List (Attribute Msg)
searchInputStyle =
    [ style "width" "100px" ]


viewResultMenu : Model -> List (Html Msg)
viewResultMenu model =
    case ( model.searchMenu, model.searchResult |> List.isEmpty ) of
        ( ResultOpen _, False ) ->
            [ div
                ([ on "click" (itemDecoder ClickItem)
                 , on "mouseover" (itemDecoder OverItem)
                 , on "mouseout" (itemDecoder OutItem)
                 , stopPropagationOnMousedown NoOp
                 ]
                    ++ resultMenuStyle
                )
                (model.searchResult
                    |> List.map
                        (\id ->
                            case getTopicInfo id model of
                                Just topic ->
                                    div
                                        ([ attribute "data-id" (fromInt topic.id) ]
                                            ++ resultItemStyle topic.id model
                                        )
                                        [ text topic.text ]

                                Nothing ->
                                    text "??"
                        )
                )
            ]

        _ ->
            []


itemDecoder : (Id -> SearchMsg) -> D.Decoder Msg
itemDecoder msg =
    D.map Search <|
        D.map msg
            (D.at [ "target", "dataset", "id" ] D.string
                |> D.andThen strToIntDecoder
            )


resultMenuStyle : List (Attribute Msg)
resultMenuStyle =
    [ style "position" "absolute"
    , style "top" "144px"
    , style "width" "240px"
    , style "padding" "3px 0"
    , style "font-size" <| fromInt contentFontSize ++ "px"
    , style "line-height" "2"
    , style "white-space" "nowrap"
    , style "background-color" "white"
    , style "border" "1px solid lightgray"
    , style "z-index" "2"
    ]


resultItemStyle : Id -> Model -> List (Attribute Msg)
resultItemStyle topicId model =
    let
        isHover =
            case model.searchMenu of
                ResultOpen maybeId ->
                    maybeId == Just topicId

                ResultClosed ->
                    False
    in
    [ style "color"
        (if isHover then
            "white"

         else
            "black"
        )
    , style "background-color"
        (if isHover then
            "black"

         else
            "white"
        )
    , style "overflow" "hidden"
    , style "text-overflow" "ellipsis"
    , style "padding" "0 8px"
    ]



-- UPDATE


updateSearch : SearchMsg -> Model -> ( Model, Cmd Msg )
updateSearch msg model =
    case msg of
        SearchInput text ->
            ( onSearchInput text model, Cmd.none )

        SearchFocus ->
            ( { model | searchMenu = ResultOpen Nothing }, Cmd.none )

        OverItem topicId ->
            ( onOverItem topicId model, Cmd.none )

        OutItem _ ->
            ( onOutItem model, Cmd.none )

        ClickItem topicId ->
            model
                |> revealTopic topicId (activeMap model)
                |> closeResultMenu
                |> storeModel


onSearchInput : String -> Model -> Model
onSearchInput text model =
    { model | searchText = text } |> search


onOverItem : Id -> Model -> Model
onOverItem topicId model =
    case model.searchMenu of
        ResultOpen _ ->
            { model | searchMenu = ResultOpen (Just topicId) }

        -- update hovered topic
        ResultClosed ->
            logError "onOverItem"
                "Received \"OverItem\" message when searchMenu is ResultClosed"
                model


onOutItem : Model -> Model
onOutItem model =
    case model.searchMenu of
        ResultOpen _ ->
            { model | searchMenu = ResultOpen Nothing }

        -- update hovered topic
        ResultClosed ->
            logError "onOutItem"
                "Received \"OutItem\" message when searchMenu is ResultClosed"
                model


search : Model -> Model
search model =
    { model
        | searchResult =
            model.items
                |> Dict.foldr
                    (\id item topicIds ->
                        case item of
                            Topic { text } ->
                                if isMatch model.searchText text then
                                    id :: topicIds

                                else
                                    topicIds

                            Assoc _ ->
                                topicIds
                    )
                    []
        , searchMenu = ResultOpen Nothing
    }


isMatch : String -> String -> Bool
isMatch searchText text =
    not (searchText |> String.isEmpty)
        && String.contains (String.toLower searchText) (String.toLower text)


revealTopic : Id -> MapId -> Model -> Model
revealTopic topicId mapId model =
    if isItemInMap topicId mapId model then
        let
            _ =
                info "revealTopic" ( topicId, "set visible" )
        in
        showItem topicId mapId model

    else
        let
            _ =
                info "revealTopic" ( topicId, "add to map" )

            props =
                MapTopic <| defaultProps topicId topicSize model
        in
        addItemToMap topicId props mapId model


closeResultMenu : Model -> Model
closeResultMenu model =
    { model | searchMenu = ResultClosed }
