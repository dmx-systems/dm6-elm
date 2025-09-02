module Main exposing (..)

import AppModel as AM
import Boxing exposing (boxContainer, unboxContainer)
import Browser
import Browser.Dom as Dom
import Config exposing (..)
import Dict
import Domain.Reparent as R
import Html exposing (Attribute, br, div, text)
import Html.Attributes exposing (id, style)
import IconMenuAPI exposing (updateIconMenu, viewIconMenu)
import Json.Decode as D
import Json.Encode as E
import MapAutoSize exposing (autoSize)
import MapRenderer exposing (viewMap)
import Model exposing (..)
import ModelAPI exposing (..)
import MouseAPI exposing (mouseHoverHandler, mouseSubs, updateMouse)
import SearchAPI exposing (updateSearch, viewResultMenu)
import Storage exposing (modelDecoder, storeModel, storeModelWith)
import String exposing (fromFloat, fromInt)
import Task
import Toolbar exposing (viewToolbar)
import Utils exposing (..)



-- MAIN


main : Program E.Value AM.Model AM.Msg
main =
    Browser.document
        { init = init
        , view = view
        , update = update
        , subscriptions = mouseSubs
        }


init : E.Value -> ( AM.Model, Cmd AM.Msg )
init flags =
    ( case flags |> D.decodeValue (D.null True) of
        Ok True ->
            let
                _ =
                    info "init" "localStorage: empty"
            in
            AM.default

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
                    AM.default
    , Cmd.none
    )



-- VIEW


view : AM.Model -> Browser.Document AM.Msg
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


appStyle : List (Attribute AM.Msg)
appStyle =
    [ style "font-family" mainFont
    , style "user-select" "none"
    , style "-webkit-user-select" "none" -- Safari still needs vendor prefix
    ]


measureStyle : List (Attribute AM.Msg)
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


update : AM.Msg -> AM.Model -> ( AM.Model, Cmd AM.Msg )
update msg model =
    let
        _ =
            case msg of
                AM.Mouse _ ->
                    msg

                _ ->
                    info "update" msg
    in
    case msg of
        AM.AddTopic ->
            createTopicAndAddToMap topicDefaultText Nothing (activeMap model) model
                |> storeModel

        AM.MoveTopicToMap topicId mapId origPos targetId targetMapId pos ->
            moveTopicToMap topicId mapId origPos targetId targetMapId pos model |> storeModel

        AM.SwitchDisplay displayMode ->
            switchDisplay displayMode model |> storeModel

        AM.Search searchMsg ->
            updateSearch searchMsg model

        AM.Edit editMsg ->
            updateEdit editMsg model

        AM.IconMenu iconMenuMsg ->
            updateIconMenu iconMenuMsg model

        AM.Mouse mouseMsg ->
            updateMouse mouseMsg model

        AM.Nav navMsg ->
            updateNav navMsg model |> storeModel

        AM.Hide ->
            hide model |> storeModel

        AM.Delete ->
            delete model |> storeModel

        AM.NoOp ->
            ( model, Cmd.none )


moveTopicToMap : Id -> MapId -> Point -> Id -> MapId -> Point -> AM.Model -> AM.Model
moveTopicToMap topicId mapId origPos targetId targetMapId pos model =
    let
        ( newModel, created ) =
            createMapIfNeeded targetId model

        newPos =
            case created of
                True ->
                    Point
                        (topicW2 + whiteBoxPadding)
                        (topicH2 + whiteBoxPadding)

                False ->
                    pos

        props_ =
            getTopicProps topicId mapId newModel.maps
                |> Maybe.andThen (\props -> Just (MapTopic { props | pos = newPos }))
    in
    case props_ of
        Just props ->
            newModel
                |> hideItem topicId mapId
                |> setTopicPos topicId mapId origPos
                |> addItemToMap topicId props targetId
                |> select targetId targetMapId
                |> autoSize

        Nothing ->
            model


createMapIfNeeded : Id -> AM.Model -> ( AM.Model, Bool )
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


setDisplayModeInAllMaps : Id -> DisplayMode -> AM.Model -> AM.Model
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


switchDisplay : DisplayMode -> AM.Model -> AM.Model
switchDisplay displayMode model =
    (case getSingleSelection model of
        Just ( containerId, targetMapId ) ->
            { model
                | maps =
                    case displayMode of
                        Monad _ ->
                            model.maps

                        Container BlackBox ->
                            boxContainer containerId targetMapId model

                        Container WhiteBox ->
                            boxContainer containerId targetMapId model

                        Container Unboxed ->
                            unboxContainer containerId targetMapId model
            }
                |> setDisplayMode containerId targetMapId displayMode

        Nothing ->
            model
    )
        |> autoSize



-- Text Edit


updateEdit : EditMsg -> AM.Model -> ( AM.Model, Cmd AM.Msg )
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


startEdit : AM.Model -> ( AM.Model, Cmd AM.Msg )
startEdit model =
    let
        newModel =
            case getSingleSelection model of
                Just ( topicId, mapId ) ->
                    { model | editState = ItemEdit topicId mapId }
                        |> setDetailDisplayIfMonade topicId mapId
                        |> autoSize

                Nothing ->
                    model
    in
    ( newModel, focus newModel )


setDetailDisplayIfMonade : Id -> MapId -> AM.Model -> AM.Model
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


onTextInput : String -> AM.Model -> AM.Model
onTextInput text model =
    case model.editState of
        ItemEdit topicId _ ->
            updateTopicInfo topicId
                (\topic -> { topic | text = text })
                model

        NoEdit ->
            logError "onTextInput" "called when editState is NoEdit" model


onTextareaInput : String -> AM.Model -> ( AM.Model, Cmd AM.Msg )
onTextareaInput text model =
    case model.editState of
        ItemEdit topicId mapId ->
            updateTopicInfo topicId
                (\topic -> { topic | text = text })
                model
                |> measureText text topicId mapId

        NoEdit ->
            logError "onTextareaInput" "called when editState is NoEdit" ( model, Cmd.none )


measureText : String -> Id -> MapId -> AM.Model -> ( AM.Model, Cmd AM.Msg )
measureText text topicId mapId model =
    ( { model | measureText = text }
    , Dom.getElement "measure"
        |> Task.attempt
            (\result ->
                case result of
                    Ok elem ->
                        AM.Edit
                            (SetTopicSize topicId
                                mapId
                                (Size elem.element.width elem.element.height)
                            )

                    Err err ->
                        logError "measureText" (toString err) AM.NoOp
            )
    )


endEdit : AM.Model -> AM.Model
endEdit model =
    { model | editState = NoEdit }
        |> autoSize


focus : AM.Model -> Cmd AM.Msg
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
                        AM.NoOp

                    Err e ->
                        logError "focus" (toString e) AM.NoOp
            )



--


updateNav : NavMsg -> AM.Model -> AM.Model
updateNav navMsg model =
    case navMsg of
        Fullscreen ->
            fullscreen model

        Back ->
            back model


fullscreen : AM.Model -> AM.Model
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


back : AM.Model -> AM.Model
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
        , selection = selection
    }
        |> adjustMapRect mapId 1
        |> autoSize


adjustMapRect : MapId -> Float -> AM.Model -> AM.Model
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


hide : AM.Model -> AM.Model
hide model =
    let
        newModel =
            model.selection
                |> List.foldr
                    (\( itemId, mapId ) modelAcc -> hideItem itemId mapId modelAcc)
                    model
    in
    { newModel | selection = [] }
        |> autoSize


delete : AM.Model -> AM.Model
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



-- OPEN-DOOR / REPARENT ADAPTER (pure invariant lives in Domain.Reparent)


canReparent : MapId -> Maybe MapId -> AM.Model -> Result String ()
canReparent a b model =
    R.canReparent a b (parentsOf model)



-- Curried so (parentsOf model) : MapId -> List MapId


parentsOf : AM.Model -> MapId -> List MapId
parentsOf model childId =
    -- TODO: derive direct parent map IDs of `childId` from your dmx.composition assocs
    []
