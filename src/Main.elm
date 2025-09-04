module Main exposing (init, main, moveTopicToMap, update, view)

import AppModel as AM exposing (..)
import Boxing exposing (boxContainer, unboxContainer)
import Browser
import Browser.Dom as Dom
import Compat.ModelAPI
import Config exposing (..)
import Dict
import Feature.Connection.Channel as Channel
import Html exposing (Attribute, br, div, text)
import Html.Attributes exposing (id, style)
import IconMenuAPI exposing (updateIconMenu, viewIconMenu)
import Json.Decode as D
import Json.Encode as E
import MapAutoSize exposing (autoSize)
import MapRenderer exposing (viewMap)
import Model as M exposing (..)
import ModelAPI
    exposing
        ( activeMap
        , createMap
        , createTopicIn
        , defaultProps
        , deleteItem
        , getMapId
        , getSingleSelection
        , getTopicProps
        , hasMap
        , hideItem
        , isItemInMap
        , select
        , setDisplayMode
        , setTopicPos
        , setTopicSize
        , updateMapRect
        , updateTopicInfo
        , updateTopicProps
        )
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
            (id "measure" :: measureStyle)
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


{-| Compatibility wrapper used by Feature.OpenDoor.\* and tests.

    moveTopicToMap topicId fromId origPos targetId parentPath pos model

-}
moveTopicToMap :
    Id
    -> Id
    -> Point
    -> Id
    -> List Id
    -> Point
    -> AM.Model
    -> AM.Model
moveTopicToMap topicId fromId origPos targetId parentPath pos model =
    let
        fromBoundary =
            if fromId == 0 then
                Channel.Root

            else
                Channel.Container fromId

        toBoundary =
            if targetId == 0 then
                Channel.Root

            else
                Channel.Container targetId

        req =
            { topicId = topicId
            , from = fromBoundary
            , to = toBoundary
            , pos = pos
            , permit = Channel.defaultPermit
            }
    in
    case Channel.cross req model of
        Ok ( model1, _, _ ) ->
            model1

        Err _ ->
            model



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

        MoveTopicToMap topicId containerId origPos targetId targetMapId pos ->
            let
                req =
                    if targetId == 0 then
                        -- cross OUT to root
                        { topicId = topicId
                        , from = Channel.Container containerId
                        , to = Channel.Root
                        , pos = pos
                        , permit = Channel.defaultPermit
                        }

                    else
                        -- cross IN to container targetId
                        { topicId = topicId
                        , from = Channel.Root
                        , to = Channel.Container targetId
                        , pos = pos
                        , permit = Channel.defaultPermit
                        }
            in
            case Channel.cross req model of
                Ok ( model1, _, eff ) ->
                    let
                        cmd =
                            case eff of
                                Channel.None ->
                                    Cmd.none

                                Channel.Out_Crossed _ ->
                                    -- no ports in tests; emit nothing
                                    Cmd.none
                    in
                    ( model1, cmd )

                Err _ ->
                    ( model, Cmd.none )

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
                if isItemInMap topicId mapId model then
                    setDisplayMode topicId mapId displayMode modelAcc

                else
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
        ( mapId, mapPath, _ ) =
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
