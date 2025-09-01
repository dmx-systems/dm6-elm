module MouseAPI exposing (mouseHoverHandler, mouseSubs, updateMouse)

-- components

import AppModel exposing (Model, Msg(..))
import Browser.Events as Events
import Config exposing (assocDelayMillis, topicH2, topicW2, whiteBoxPadding, whiteBoxRange)
import Html exposing (Attribute)
import Html.Events exposing (on)
import IconMenuAPI exposing (closeIconMenu)
import Json.Decode as D
import MapAutoSize exposing (autoSize)
import Model exposing (Class, Id, MapPath, Point)
import ModelAPI
    exposing
        ( createDefaultAssoc
        , fromPath
        , getMapId
        , getTopicPos
        , idDecoder
        , pathDecoder
        , select
        , setTopicPosByDelta
        )
import Mouse exposing (DragMode(..), DragState(..), MouseMsg(..))
import Random
import SearchAPI exposing (closeResultMenu)
import Storage exposing (storeModelWith)
import String exposing (fromInt)
import Task
import Time exposing (posixToMillis)
import Utils exposing (info, logError, toString)



-- VIEW


mouseHoverHandler : List (Attribute Msg)
mouseHoverHandler =
    [ on "mouseover" (mouseDecoder Over)
    , on "mouseout" (mouseDecoder Out)
    ]



-- UPDATE


updateMouse : MouseMsg -> Model -> ( Model, Cmd Msg )
updateMouse msg model =
    case msg of
        Down ->
            ( mouseDown model, Cmd.none )

        DownItem class id mapPath pos ->
            mouseDownOnItem model class id mapPath pos

        Move pos ->
            mouseMove model pos

        Up ->
            mouseUp model |> storeModelWith

        Over class id mapPath ->
            ( mouseOver model class id mapPath, Cmd.none )

        Out class id mapPath ->
            ( mouseOut model class id mapPath, Cmd.none )

        Time time ->
            ( timeArrived time model, Cmd.none )


mouseDown : Model -> Model
mouseDown model =
    { model | selection = [] }
        |> closeIconMenu
        |> closeResultMenu


mouseDownOnItem : Model -> Class -> Id -> MapPath -> Point -> ( Model, Cmd Msg )
mouseDownOnItem model class id mapPath pos =
    ( updateDragState model (WaitForStartTime class id mapPath pos)
        |> select id (getMapId mapPath)
    , Task.perform (Mouse << Time) Time.now
    )


timeArrived : Time.Posix -> Model -> Model
timeArrived time model =
    case model.mouse.dragState of
        WaitForStartTime class id mapPath pos ->
            updateDragState model <| DragEngaged time class id mapPath pos

        WaitForEndTime startTime class id mapPath pos ->
            updateDragState model <|
                case class of
                    "dmx-topic" ->
                        let
                            delay =
                                posixToMillis time - posixToMillis startTime > assocDelayMillis

                            dragMode =
                                if delay then
                                    DrawAssoc

                                else
                                    DragTopic

                            mapId =
                                getMapId mapPath

                            origPos_ =
                                getTopicPos id mapId model.maps
                        in
                        case origPos_ of
                            Just origPos ->
                                Drag dragMode id mapPath origPos pos Nothing

                            Nothing ->
                                NoDrag

                    _ ->
                        NoDrag

        -- the error will be logged in performDrag
        _ ->
            logError "timeArrived"
                "Received \"Time\" message when dragState is not WaitForTime"
                model


mouseMove : Model -> Point -> ( Model, Cmd Msg )
mouseMove model pos =
    case model.mouse.dragState of
        DragEngaged time class id mapPath pos_ ->
            ( updateDragState model <| WaitForEndTime time class id mapPath pos_
            , Task.perform (Mouse << Time) Time.now
            )

        WaitForEndTime _ _ _ _ _ ->
            ( model, Cmd.none )

        -- ignore -- TODO: can this happen at all? Is there a move listener?
        Drag _ _ _ _ _ _ ->
            ( performDrag model pos, Cmd.none )

        _ ->
            logError "mouseMove"
                ("Received \"Move\" message when dragState is " ++ toString model.mouse.dragState)
                ( model, Cmd.none )


performDrag : Model -> Point -> Model
performDrag model pos =
    case model.mouse.dragState of
        Drag dragMode id mapPath origPos lastPos target ->
            let
                delta =
                    Point
                        (pos.x - lastPos.x)
                        (pos.y - lastPos.y)

                mapId =
                    getMapId mapPath

                newModel =
                    case dragMode of
                        DragTopic ->
                            setTopicPosByDelta id mapId delta model

                        DrawAssoc ->
                            model
            in
            -- update lastPos
            updateDragState newModel (Drag dragMode id mapPath origPos pos target)
                |> autoSize

        _ ->
            logError "performDrag"
                ("Received \"Move\" message when dragState is " ++ toString model.mouse.dragState)
                model


mouseUp : Model -> ( Model, Cmd Msg )
mouseUp model =
    let
        ( newModel, cmd ) =
            case model.mouse.dragState of
                Drag DragTopic id mapPath origPos _ (Just ( targetId, targetMapId )) ->
                    let
                        _ =
                            info "mouseUp"
                                ("dropped "
                                    ++ fromInt id
                                    ++ " (map "
                                    ++ fromPath mapPath
                                    ++ ") on "
                                    ++ fromInt targetId
                                    ++ " (map "
                                    ++ fromInt targetMapId
                                    ++ ") --> "
                                    ++ (if notDroppedOnOwnMap then
                                            "move topic"

                                        else
                                            "abort"
                                       )
                                )

                        mapId =
                            getMapId mapPath

                        notDroppedOnOwnMap =
                            mapId /= targetId

                        msg =
                            MoveTopicToMap id mapId origPos targetId targetMapId
                    in
                    if notDroppedOnOwnMap then
                        ( model, Random.generate msg point )

                    else
                        ( model, Cmd.none )

                Drag DrawAssoc id mapPath _ _ (Just ( targetId, targetMapId )) ->
                    let
                        _ =
                            info "mouseUp"
                                ("assoc drawn from "
                                    ++ fromInt id
                                    ++ " (map "
                                    ++ fromPath
                                        mapPath
                                    ++ ") to "
                                    ++ fromInt targetId
                                    ++ " (map "
                                    ++ fromInt targetMapId
                                    ++ ") --> "
                                    ++ (if isSameMap then
                                            "create assoc"

                                        else
                                            "abort"
                                       )
                                )

                        mapId =
                            getMapId mapPath

                        isSameMap =
                            mapId == targetMapId
                    in
                    if isSameMap then
                        ( createDefaultAssoc id targetId mapId model, Cmd.none )

                    else
                        ( model, Cmd.none )

                Drag _ _ _ _ _ _ ->
                    let
                        _ =
                            info "mouseUp" "drag ended w/o target"
                    in
                    ( model, Cmd.none )

                DragEngaged _ _ _ _ _ ->
                    let
                        _ =
                            info "mouseUp" "drag aborted w/o moving"
                    in
                    ( model, Cmd.none )

                _ ->
                    logError "mouseUp"
                        ("Received \"Up\" message when dragState is " ++ toString model.mouse.dragState)
                        ( model, Cmd.none )
    in
    ( updateDragState newModel NoDrag, cmd )


point : Random.Generator Point
point =
    let
        cx =
            topicW2 + whiteBoxPadding

        cy =
            topicH2 + whiteBoxPadding

        rw =
            whiteBoxRange.w

        rh =
            whiteBoxRange.h
    in
    Random.map2
        (\x y -> Point (cx + x) (cy + y))
        (Random.float 0 rw)
        (Random.float 0 rh)


mouseOver : Model -> Class -> Id -> MapPath -> Model
mouseOver model class targetId targetMapPath =
    case model.mouse.dragState of
        Drag dragMode id mapPath origPos lastPos _ ->
            let
                mapId =
                    getMapId mapPath

                targetMapId =
                    getMapId targetMapPath

                target =
                    if ( id, mapId ) /= ( targetId, targetMapId ) then
                        Just ( targetId, targetMapId )

                    else
                        Nothing
            in
            -- update target
            updateDragState model <| Drag dragMode id mapPath origPos lastPos target

        DragEngaged _ _ _ _ _ ->
            logError "mouseOver" "Received \"Over\" message when dragState is DragEngaged" model

        _ ->
            model


mouseOut : Model -> Class -> Id -> MapPath -> Model
mouseOut model class targetId targetMapPath =
    case model.mouse.dragState of
        Drag dragMode id mapId origPos lastPos _ ->
            -- reset target
            updateDragState model <| Drag dragMode id mapId origPos lastPos Nothing

        _ ->
            model


updateDragState : Model -> DragState -> Model
updateDragState ({ mouse } as model) dragState =
    { model | mouse = { mouse | dragState = dragState } }



-- SUBSCRIPTIONS


mouseSubs : Model -> Sub Msg
mouseSubs model =
    case model.mouse.dragState of
        WaitForStartTime _ _ _ _ ->
            Sub.none

        WaitForEndTime _ _ _ _ _ ->
            Sub.none

        DragEngaged _ _ _ _ _ ->
            dragSub

        Drag _ _ _ _ _ _ ->
            dragSub

        NoDrag ->
            mouseDownSub


mouseDownSub : Sub Msg
mouseDownSub =
    Events.onMouseDown <|
        D.oneOf
            [ D.map Mouse <|
                D.map4 DownItem
                    (D.oneOf
                        [ D.at [ "target", "className" ] D.string -- HTML elements
                        , D.at [ "target", "className", "baseVal" ] D.string -- SVG elements
                        ]
                    )
                    (D.at [ "target", "dataset", "id" ] D.string |> D.andThen idDecoder)
                    (D.at [ "target", "dataset", "path" ] D.string |> D.andThen pathDecoder)
                    (D.map2 Point
                        -- TODO: no code doubling
                        (D.field "clientX" D.float)
                        (D.field "clientY" D.float)
                    )
            , D.succeed (Mouse Down)
            ]


dragSub : Sub Msg
dragSub =
    Sub.batch
        [ Events.onMouseMove <|
            D.map Mouse <|
                D.map Move
                    (D.map2 Point
                        -- TODO: no code doubling
                        (D.field "clientX" D.float)
                        (D.field "clientY" D.float)
                    )
        , Events.onMouseUp <| D.map Mouse <| D.succeed Up
        ]



-- TODO: no code doubling


mouseDecoder : (Class -> Id -> MapPath -> MouseMsg) -> D.Decoder Msg
mouseDecoder msg =
    D.map Mouse <|
        D.map3 msg
            (D.oneOf
                [ D.at [ "target", "className" ] D.string -- HTML elements
                , D.at [ "target", "className", "baseVal" ] D.string -- SVG elements
                ]
            )
            (D.at [ "target", "dataset", "id" ] D.string |> D.andThen idDecoder)
            (D.at [ "target", "dataset", "path" ] D.string |> D.andThen pathDecoder)
