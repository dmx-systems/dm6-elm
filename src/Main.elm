module Main exposing (..)

-- for Html.map

import AppModel exposing (..)
import Boxing exposing (boxContainer, unboxContainer)
import Browser
import Browser.Dom as Dom
import Config exposing (..)
import Dict
import Feature.Cross as Cross
import Feature.OpenDoor.Move as OpenDoor
import Html exposing (Attribute, Html, br, div, text)
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



-- MAIN MESSAGE WRAPPER


type MainMsg
    = App AppModel.Msg
    | CrossMsg Cross.Msg



-- MAIN


main : Program E.Value AppModel.Model MainMsg
main =
    Browser.document
        { init = init
        , view = view
        , update = update
        , subscriptions = \m -> Sub.map App (mouseSubs m)
        }


init : E.Value -> ( Model, Cmd MainMsg )
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


view : AppModel.Model -> Browser.Document MainMsg
view model =
    Browser.Document
        "DM6 Elm"
        [ -- Cross button (feature-local msg wrapped into MainMsg)
          Html.map CrossMsg Cross.view
        , -- Main application UI mapped as one subtree
          Html.map App <|
            div
                (mouseHoverHandler ++ appStyle)
                ([ viewToolbar model
                 , viewMap (activeMap model) -1 model -- parentMapId = -1
                 ]
                    ++ viewResultMenu model
                    ++ viewIconMenu model
                )
        , -- hidden measurement node (also under App mapping)
          Html.map App <|
            div
                ([ id "measure" ] ++ measureStyle)
                [ text model.measureText
                , br [] []
                ]
        ]


appStyle : List (Attribute AppModel.Msg)
appStyle =
    [ style "font-family" mainFont
    , style "user-select" "none"
    , style "-webkit-user-select" "none" -- Safari still needs vendor prefix
    ]


measureStyle : List (Attribute AppModel.Msg)
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


update : MainMsg -> AppModel.Model -> ( AppModel.Model, Cmd MainMsg )
update msg model =
    let
        -- map AppModel commands into MainMsg
        mapApp : ( Model, Cmd AppModel.Msg ) -> ( Model, Cmd MainMsg )
        mapApp =
            Tuple.mapSecond (Cmd.map App)
    in
    case msg of
        -- Cross button: move selected topic out of container to its parent map
        CrossMsg Cross.CrossClick ->
            case getSingleSelection model of
                Just ( topicId, containerId ) ->
                    let
                        parentMapId =
                            case Dict.get containerId model.maps of
                                Just containerMap ->
                                    containerMap.parentMapId

                                Nothing ->
                                    0
                    in
                    OpenDoor.move
                        { containerId = containerId
                        , topicId = topicId
                        , targetMapId = parentMapId
                        }
                        model
                        |> storeModel
                        |> mapApp

                Nothing ->
                    ( model, Cmd.none )

        -- All existing app messages are handled as before, then mapped
        App appMsg ->
            mapApp <|
                case appMsg of
                    AddTopic ->
                        createTopicAndAddToMap topicDefaultText Nothing (activeMap model) model
                            |> storeModel

                    MoveTopicToMap topicId mapId origPos targetId targetMapId pos ->
                        moveTopicToMap topicId mapId origPos targetId targetMapId pos model
                            |> storeModel

                    SwitchDisplay displayMode ->
                        switchDisplay displayMode model
                            |> storeModel

                    Search searchMsg ->
                        updateSearch searchMsg model

                    Edit editMsg ->
                        updateEdit editMsg model

                    IconMenu iconMenuMsg ->
                        updateIconMenu iconMenuMsg model

                    Mouse mouseMsg ->
                        updateMouse mouseMsg model

                    Nav navMsg ->
                        updateNav navMsg model
                            |> storeModel

                    Hide ->
                        hide model
                            |> storeModel

                    Delete ->
                        delete model
                            |> storeModel

                    NoOp ->
                        ( model, Cmd.none )



-- Original helpers remain unchanged (they operate on AppModel.Msg/Model)


moveTopicToMap : Id -> MapId -> Point -> Id -> MapId -> Point -> Model -> Model
moveTopicToMap topicId mapId origPos targetId targetMapId pos model =
    let
        ( newModel, created ) =
            createMapIfNeeded targetId targetMapId model

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


createMapIfNeeded : Id -> MapId -> Model -> ( Model, Bool )
createMapIfNeeded topicId mapId model =
    if hasMap topicId model.maps then
        ( model, False )

    else
        ( { model
            | maps =
                model.maps
                    |> Dict.insert
                        topicId
                        (Map topicId Dict.empty (Rectangle 0 0 0 0) mapId)
          }
            |> setDisplayMode topicId mapId (Container BlackBox)
        , True
        )


switchDisplay : DisplayMode -> Model -> Model
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


updateEdit : EditMsg -> Model -> ( Model, Cmd AppModel.Msg )
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


startEdit : Model -> ( Model, Cmd AppModel.Msg )
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


onTextareaInput : String -> Model -> ( Model, Cmd AppModel.Msg )
onTextareaInput text model =
    case model.editState of
        ItemEdit topicId mapId ->
            updateTopicInfo topicId
                (\topic -> { topic | text = text })
                model
                |> measureText text topicId mapId

        NoEdit ->
            logError "onTextareaInput" "called when editState is NoEdit" ( model, Cmd.none )


measureText : String -> Id -> MapId -> Model -> ( Model, Cmd AppModel.Msg )
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


focus : Model -> Cmd AppModel.Msg
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
        Just ( topicId, mapId ) ->
            { model
                | mapPath = topicId :: model.mapPath
                , selection = []
            }
                |> createMapIfNeeded topicId mapId
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
        , selection = selection
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
                    (\( itemId, mapId ) modelAcc -> hideItem itemId mapId modelAcc)
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
