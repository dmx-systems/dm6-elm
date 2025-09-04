module Main exposing (..)

import AppModel exposing (..)
import Boxing exposing (boxContainer, unboxContainer)
import Browser
import Browser.Dom as Dom
import Compat.ModelAPI as ModelAPI exposing (addItemToMap)
import Config exposing (..)
import Dict
import Html exposing (Attribute, br, div, text)
import Html.Attributes exposing (id, style)
import IconMenuAPI exposing (updateIconMenu, viewIconMenu)
import Json.Decode as D
import Json.Encode as E
import MapAutoSize exposing (autoSize)
import MapRenderer exposing (viewMap)
import Model exposing (..)
import ModelAPI exposing (activeMap, createMap, createTopicIn, deleteItem, getMapId, getSingleSelection, getTopicProps, hasMap, hideItem, isItemInMap, select, setDisplayMode, setTopicPos, setTopicSize, updateMapRect, updateTopicInfo, updateTopicProps)
import MouseAPI exposing (mouseHoverHandler, mouseSubs, updateMouse)
import SearchAPI exposing (updateSearch, viewResultMenu)
import Storage exposing (modelDecoder, storeModel, storeModelWith)
import String exposing (fromFloat, fromInt)
import Task
import UI.Toolbar exposing (viewToolbar)
import Utils exposing (..)



-- MAIN


main : Program E.Value Model Msg
main =
    Browser.document
        { init = init
        , view = view
        , update = update
        , subscriptions = mouseSubs
        }


init : E.Value -> ( Model, Cmd Msg )
init flags =
    ( case flags |> D.decodeValue (D.null True) of
        Ok True ->
            let
                _ =
                    info "init" "localStorage: empty"
            in
            default

        _ ->
            case flags |> D.decodeValue modelDecoder of
                Ok model ->
                    let
                        _ =
                            info "init"
                                ("localStorage: " ++ (model |> toString |> String.length |> fromInt) ++ " bytes")
                    in
                    model

                Err e ->
                    let
                        _ =
                            logError "init" "localStorage" e
                    in
                    default
    , Cmd.none
    )



-- VIEW


view : Model -> Browser.Document Msg
view model =
    Browser.Document
        "DM6 Elm"
        [ div
            (mouseHoverHandler
                ++ appStyle
            )
            ([ viewToolbar model
             , viewMap (activeMap model) [] model -- mapPath = []
             ]
                ++ viewResultMenu model
                ++ viewIconMenu model
            )
        , div
            ([ id "measure" ]
                ++ measureStyle
            )
            [ text model.measureText
            , br [] []
            ]
        ]


appStyle : List (Attribute Msg)
appStyle =
    [ style "font-family" mainFont
    , style "user-select" "none"
    , style "-webkit-user-select" "none" -- Safari still needs vendor prefix
    ]


measureStyle : List (Attribute Msg)
measureStyle =
    [ style "position" "fixed"
    , style "visibility" "hidden"
    , style "white-space" "pre-wrap"
    , style "font-family" mainFont
    , style "font-size" <| fromInt contentFontSize ++ "px"
    , style "line-height" <| fromFloat topicLineHeight
    , style "padding" <| fromInt topicDetailPadding ++ "px"
    , style "width" <| fromFloat topicDetailMaxWidth ++ "px"
    , style "min-width" <| fromFloat (topicSize.w - topicSize.h) ++ "px"
    , style "max-width" "max-content"
    , style "border-width" <| fromFloat topicBorderWidth ++ "px"
    , style "border-style" "solid"
    , style "box-sizing" "border-box"
    ]



-- UPDATE


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        _ =
            case msg of
                Mouse _ ->
                    msg

                _ ->
                    info "update" msg
    in
    case msg of
        AddTopic ->
            createTopicIn topicDefaultText Nothing [ activeMap model ] model |> storeModel

        MoveTopicToMap topicId mapId origPos targetId targetMapPath pos ->
            moveTopicToMap topicId mapId origPos targetId targetMapPath pos model |> storeModel

        SwitchDisplay displayMode ->
            switchDisplay displayMode model |> storeModel

        Search searchMsg ->
            updateSearch searchMsg model

        Edit editMsg ->
            updateEdit editMsg model

        IconMenu iconMenuMsg ->
            updateIconMenu iconMenuMsg model

        Mouse mouseMsg ->
            updateMouse mouseMsg model

        Nav navMsg ->
            updateNav navMsg model |> storeModel

        Hide ->
            hide model |> storeModel

        Delete ->
            delete model |> storeModel

        NoOp ->
            ( model, Cmd.none )



-- Derive targetMapId from the path; upstream createMapIfNeeded takes 2 args


moveTopicToMap : Id -> MapId -> Point -> Id -> List MapId -> Point -> Model -> Model
moveTopicToMap topicId sourceMapId origPos targetId targetMapPath newPos model0 =
    let
        isSelfTarget =
            targetId == topicId

        -- ensure destination map exists (container’s inner map or background)
        ( model1, created ) =
            createMapIfNeeded targetId model0

        actualPos =
            if created then
                Point (topicW2 + whiteBoxPadding) (topicH2 + whiteBoxPadding)

            else
                newPos

        -- find the real source map that currently holds the topic
        findSourceAndProps : Maybe ( MapId, TopicProps )
        findSourceAndProps =
            case getTopicProps topicId sourceMapId model1.maps of
                Just tp ->
                    Just ( sourceMapId, tp )

                Nothing ->
                    model1.maps
                        |> Dict.toList
                        |> List.filterMap
                            (\( mid, _ ) ->
                                getTopicProps topicId mid model1.maps
                                    |> Maybe.map (\tp -> ( mid, tp ))
                            )
                        |> List.head
    in
    if isSelfTarget then
        model0

    else
        case findSourceAndProps of
            Just ( realSourceMapId, tp ) ->
                let
                    props =
                        MapTopic { tp | pos = actualPos }
                in
                model1
                    |> hideItem topicId realSourceMapId
                    |> setTopicPos topicId realSourceMapId origPos
                    |> addItemToMap topicId props targetId
                    |> select targetId targetMapPath
                    |> autoSize

            Nothing ->
                model0


createMapIfNeeded : Id -> Model -> ( Model, Bool )
createMapIfNeeded topicId model =
    if hasMap topicId model.maps then
        ( model, False )

    else
        ( model
            |> createMap topicId
            |> setDisplayModeInAllMaps topicId (Container BlackBox)
          -- A nested topic which becomes a container might exist in other maps as well, still as
          -- a monad. We must set the topic's display mode to "container" in *all* maps. Otherwise
          -- in the other maps it might be revealed still as a monad.
        , True
        )


setDisplayModeInAllMaps : Id -> DisplayMode -> Model -> Model
setDisplayModeInAllMaps topicId displayMode model =
    model.maps
        |> Dict.foldr
            (\mapId _ modelAcc ->
                case isItemInMap topicId mapId model of
                    True ->
                        setDisplayMode topicId mapId displayMode modelAcc

                    False ->
                        modelAcc
            )
            model


switchDisplay : DisplayMode -> Model -> Model
switchDisplay displayMode model =
    (case getSingleSelection model of
        Just ( containerId, mapPath ) ->
            let
                mapId =
                    getMapId mapPath
            in
            { model
                | maps =
                    case displayMode of
                        Monad _ ->
                            model.maps

                        Container BlackBox ->
                            boxContainer containerId mapId model

                        Container WhiteBox ->
                            boxContainer containerId mapId model

                        Container Unboxed ->
                            unboxContainer containerId mapId model
            }
                |> setDisplayMode containerId mapId displayMode

        Nothing ->
            model
    )
        |> autoSize



-- Text Edit


updateEdit : EditMsg -> Model -> ( Model, Cmd Msg )
updateEdit msg model =
    case msg of
        EditStart ->
            startEdit model

        OnTextInput text ->
            onTextInput text model |> storeModel

        OnTextareaInput text ->
            onTextareaInput text model |> storeModelWith

        SetTopicSize topicId mapId size ->
            ( model
                |> setTopicSize topicId mapId size
                |> autoSize
            , Cmd.none
            )

        EditEnd ->
            ( endEdit model, Cmd.none )


startEdit : Model -> ( Model, Cmd Msg )
startEdit model =
    let
        newModel =
            case getSingleSelection model of
                Just ( topicId, mapPath ) ->
                    { model | editState = ItemEdit topicId (getMapId mapPath) }
                        |> setDetailDisplayIfMonade topicId (getMapId mapPath)
                        |> autoSize

                Nothing ->
                    model
    in
    ( newModel, focus newModel )


setDetailDisplayIfMonade : Id -> MapId -> Model -> Model
setDetailDisplayIfMonade topicId mapId model =
    model
        |> updateTopicProps topicId
            mapId
            (\props ->
                case props.displayMode of
                    Monad _ ->
                        { props | displayMode = Monad Detail }

                    _ ->
                        props
            )


onTextInput : String -> Model -> Model
onTextInput text model =
    case model.editState of
        ItemEdit topicId _ ->
            updateTopicInfo topicId
                (\topic -> { topic | text = text })
                model

        NoEdit ->
            logError "onTextInput" "called when editState is NoEdit" model


onTextareaInput : String -> Model -> ( Model, Cmd Msg )
onTextareaInput text model =
    case model.editState of
        ItemEdit topicId mapId ->
            updateTopicInfo topicId
                (\topic -> { topic | text = text })
                model
                |> measureText text topicId mapId

        NoEdit ->
            logError "onTextareaInput" "called when editState is NoEdit" ( model, Cmd.none )


measureText : String -> Id -> MapId -> Model -> ( Model, Cmd Msg )
measureText text topicId mapId model =
    ( { model | measureText = text }
    , Dom.getElement "measure"
        |> Task.attempt
            (\result ->
                case result of
                    Ok elem ->
                        Edit
                            (SetTopicSize topicId
                                mapId
                                (Size elem.element.width elem.element.height)
                            )

                    Err err ->
                        logError "measureText" (toString err) NoOp
            )
    )


endEdit : Model -> Model
endEdit model =
    { model | editState = NoEdit }
        |> autoSize


focus : Model -> Cmd Msg
focus model =
    let
        nodeId =
            case model.editState of
                ItemEdit id mapId ->
                    "dmx-input-" ++ fromInt id ++ "-" ++ fromInt mapId

                NoEdit ->
                    logError "focus" "called when editState is NoEdit" ""
    in
    Dom.focus nodeId
        |> Task.attempt
            (\result ->
                case result of
                    Ok () ->
                        NoOp

                    Err e ->
                        logError "focus" (toString e) NoOp
            )



--


updateNav : NavMsg -> Model -> Model
updateNav navMsg model =
    case navMsg of
        Fullscreen ->
            fullscreen model

        Back ->
            back model


fullscreen : Model -> Model
fullscreen model =
    case getSingleSelection model of
        Just ( topicId, _ ) ->
            { model
                | mapPath = topicId :: model.mapPath
                , selection = []
            }
                |> createMapIfNeeded topicId
                |> Tuple.first
                |> adjustMapRect topicId -1

        Nothing ->
            model


back : Model -> Model
back model =
    let
        ( mapId, mapPath, selection ) =
            case model.mapPath of
                prevMapId :: nextMapId :: mapIds ->
                    ( prevMapId
                    , nextMapId :: mapIds
                    , [ ( prevMapId, nextMapId ) ]
                    )

                _ ->
                    logError "back" "model.mapPath has a problem" ( 0, [ 0 ], [] )
    in
    { model
        | mapPath = mapPath

        -- , selection = selection -- TODO
    }
        |> adjustMapRect mapId 1
        |> autoSize


adjustMapRect : MapId -> Float -> Model -> Model
adjustMapRect mapId factor model =
    model
        |> updateMapRect mapId
            (\rect ->
                Rectangle
                    (rect.x1 + factor * 400)
                    -- TODO
                    (rect.y1 + factor * 300)
                    -- TODO
                    rect.x2
                    rect.y2
            )


hide : Model -> Model
hide model =
    let
        newModel =
            model.selection
                |> List.foldr
                    (\( itemId, mapPath ) modelAcc -> hideItem itemId (getMapId mapPath) modelAcc)
                    model
    in
    { newModel | selection = [] }
        |> autoSize


delete : Model -> Model
delete model =
    let
        newModel =
            model.selection
                |> List.map Tuple.first
                |> List.foldr
                    (\itemId modelAcc -> deleteItem itemId modelAcc)
                    model
    in
    { newModel | selection = [] }
        |> autoSize
