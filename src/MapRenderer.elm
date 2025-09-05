module MapRenderer exposing (viewMap)

import AppModel exposing (..)
import Compat.ModelAPI exposing (getMapItemById)
import Config exposing (..)
import Dict
import Html exposing (Attribute, Html, div, input, text, textarea)
import Html.Attributes exposing (attribute, id, style, value)
import Html.Events exposing (onBlur, onInput)
import IconMenuAPI exposing (viewTopicIcon)
import Model exposing (..)
import ModelAPI
    exposing
        ( activeMap
        , defaultProps
        , fromPath
        , getMap
        , getMapId
        , getTopicLabel
        , getTopicPos
        , getTopicSize
        , isFullscreen
        , isItemInMap
        , isSelected
        , isVisible
        )
import Mouse exposing (DragMode(..), DragState(..))
import Search exposing (ResultMenu(..))
import String exposing (fromFloat, fromInt)
import Svg exposing (Svg, circle, g, path, svg, text)
import Svg.Attributes
    exposing
        ( cx
        , cy
        , d
        , dominantBaseline
        , fill
        , fillOpacity
        , fontFamily
        , fontSize
        , fontWeight
        , height
        , r
        , stroke
        , strokeDasharray
        , strokeWidth
        , textAnchor
        , transform
        , width
        , x
        , x1
        , x2
        , y
        , y1
        , y2
        )
import Utils exposing (..)



-- CONFIG


lineFunc : Maybe AssocInfo -> Point -> Point -> Svg Msg
lineFunc =
    taxiLine



-- directLine
-- MODEL


type alias MapInfo =
    ( ( List (Html Msg), List (Svg Msg), List (Svg Msg) )
    , Rectangle
    , ( { w : String, h : String }
      , List (Attribute Msg)
      )
    )


type alias TopicRendering =
    ( List (Attribute Msg), List (Html Msg) )



-- VIEW
-- For a fullscreen map mapPath is empty


viewMap : MapId -> MapPath -> Model -> Html Msg
viewMap mapId mapPath model =
    let
        ( ( topicsHtml, assocsSvg, topicsSvg ), mapRect, ( svgSize, mapStyle ) ) =
            mapInfo mapId mapPath model
    in
    div
        mapStyle
        [ div
            (topicLayerStyle mapRect)
            (topicsHtml
                ++ limboTopic mapId model
            )
        , svg
            ([ width svgSize.w, height svgSize.h ]
                ++ topicAttr mapId mapPath model
                ++ svgStyle
            )
            [ g
                (gAttr mapId mapRect model)
                (assocsSvg
                    ++ topicsSvg
                    ++ viewLimboAssoc mapId model
                )
            ]
        ]


gAttr : MapId -> Rectangle -> Model -> List (Attribute Msg)
gAttr _ mapRect _ =
    [ transform <|
        "translate("
            ++ fromFloat -mapRect.x1
            ++ " "
            ++ fromFloat -mapRect.y1
            ++ ")"
    ]


mapInfo : MapId -> MapPath -> Model -> MapInfo
mapInfo mapId mapPath model =
    let
        parentMapId =
            getMapId mapPath
    in
    case getMap mapId model.maps of
        Just map ->
            ( mapItems map mapPath model
            , map.rect
            , if isFullscreen mapId model then
                ( { w = "100%", h = "100%" }, [] )

              else
                ( { w = (map.rect.x2 - map.rect.x1) |> round |> fromInt
                  , h = (map.rect.y2 - map.rect.y1) |> round |> fromInt
                  }
                , whiteBoxStyle mapId map.rect parentMapId model
                )
            )

        Nothing ->
            ( ( [], [], [] ), Rectangle 0 0 0 0, ( { w = "0", h = "0" }, [] ) )


mapItems : Map -> MapPath -> Model -> ( List (Html Msg), List (Svg Msg), List (Svg Msg) )
mapItems map mapPath model =
    let
        newPath =
            map.id :: mapPath
    in
    map.items
        |> Dict.values
        |> List.filter isVisible
        |> List.foldr
            (\{ id, props } ( htmlTopics, assocs, topicsSvg ) ->
                case model.items |> Dict.get id of
                    Just { info } ->
                        case ( info, props ) of
                            ( Topic topic, MapTopic tProps ) ->
                                case effectiveDisplayMode topic.id tProps.displayMode model of
                                    Monad LabelOnly ->
                                        ( htmlTopics
                                        , assocs
                                        , viewTopicSvg topic tProps newPath model :: topicsSvg
                                        )

                                    Monad Detail ->
                                        ( viewTopic topic tProps newPath model :: htmlTopics
                                        , assocs
                                        , topicsSvg
                                        )

                                    _ ->
                                        ( viewTopic topic tProps newPath model :: htmlTopics
                                        , assocs
                                        , topicsSvg
                                        )

                            _ ->
                                logError "mapItems" ("problem with item " ++ fromInt id) ( htmlTopics, assocs, topicsSvg )

                    _ ->
                        logError "mapItems" ("problem with item " ++ fromInt id) ( htmlTopics, assocs, topicsSvg )
            )
            ( [], [], [] )


limboTopic : MapId -> Model -> List (Html Msg)
limboTopic mapId model =
    let
        activeMapId =
            activeMap model
    in
    if mapId == activeMapId then
        case model.search.menu of
            Open (Just topicId) ->
                if isItemInMap topicId activeMapId model then
                    case getMapItemById topicId activeMapId model.maps of
                        Just mapItem ->
                            if mapItem.hidden then
                                case model.items |> Dict.get topicId of
                                    Just { info } ->
                                        case ( info, mapItem.props ) of
                                            ( Topic topic, MapTopic props ) ->
                                                case effectiveDisplayMode topic.id props.displayMode model of
                                                    Monad LabelOnly ->
                                                        []

                                                    -- circle handled in SVG layer
                                                    Monad Detail ->
                                                        [ viewTopic topic props [] model ]

                                                    -- keep rich HTML detail
                                                    _ ->
                                                        [ viewTopic topic props [] model ]

                                            _ ->
                                                []

                                    _ ->
                                        []

                            else
                                -- already visible → nothing in limbo
                                []

                        Nothing ->
                            []

                else
                    -- not yet in map: render a preview for containers only
                    let
                        props =
                            defaultProps topicId topicSize model
                    in
                    case model.items |> Dict.get topicId of
                        Just { info } ->
                            case info of
                                Topic topic ->
                                    case effectiveDisplayMode topic.id props.displayMode model of
                                        Monad LabelOnly ->
                                            []

                                        -- circle handled in SVG layer
                                        Monad Detail ->
                                            [ viewTopic topic props [] model ]

                                        -- keep rich HTML detail
                                        _ ->
                                            [ viewTopic topic props [] model ]

                                _ ->
                                    []

                        _ ->
                            []

            _ ->
                []

    else
        []


viewTopicSvg : TopicInfo -> TopicProps -> MapPath -> Model -> Svg Msg
viewTopicSvg topic props mapPath model =
    let
        mapId =
            getMapId mapPath

        mark =
            monadMark topic.text

        rVal : Float
        rVal =
            (topicSize.h / 2) - topicBorderWidth

        rStr =
            fromFloat rVal

        cxStr =
            fromFloat props.pos.x

        cyStr =
            fromFloat props.pos.y

        dash =
            if isTargeted topic.id mapId model then
                "4 2"

            else
                "0"

        selected =
            isSelected topic.id mapId model

        shadowNodes : List (Svg Msg)
        shadowNodes =
            if selected then
                [ circle
                    [ cx cxStr
                    , cy cyStr
                    , r rStr
                    , fill "black"
                    , fillOpacity "0.20"
                    , transform "translate(5,5)"
                    ]
                    []
                ]

            else
                []

        mainNodes : List (Svg Msg)
        mainNodes =
            [ circle
                [ cx cxStr
                , cy cyStr
                , r rStr
                , fill "white"
                , stroke "black"
                , strokeWidth (fromFloat topicBorderWidth ++ "px")
                , strokeDasharray dash
                ]
                []
            , Svg.text_
                [ x cxStr
                , y cyStr
                , textAnchor "middle"
                , dominantBaseline "central"
                , fontFamily mainFont
                , fontSize (fromInt contentFontSize ++ "px")
                , fontWeight topicLabelWeight
                , fill "black"
                ]
                [ Svg.text mark ]
            , Svg.title [] [ Svg.text (getTopicLabel topic) ]
            ]
    in
    g
        (topicAttr topic.id mapPath model)
        (shadowNodes ++ mainNodes)


isTargeted : Id -> MapId -> Model -> Bool
isTargeted topicId mapId model =
    case model.mouse.dragState of
        Drag DragTopic _ (mapId_ :: _) _ _ target ->
            isTarget topicId mapId target && mapId_ /= topicId

        Drag DrawAssoc _ (mapId_ :: _) _ _ target ->
            isTarget topicId mapId target && mapId_ == mapId

        _ ->
            False


viewTopic : TopicInfo -> TopicProps -> MapPath -> Model -> Html Msg
viewTopic topic props mapPath model =
    let
        topicFunc =
            case effectiveDisplayMode topic.id props.displayMode model of
                Monad Detail ->
                    detailTopic

                Container BlackBox ->
                    blackBoxTopic

                Container WhiteBox ->
                    whiteBoxTopic

                Container Unboxed ->
                    unboxedTopic

                _ ->
                    -- fallback if ever called; keep labelTopic safe
                    labelTopic

        ( style, children ) =
            topicFunc topic props mapPath model
    in
    div
        (topicAttr topic.id mapPath model
            ++ topicStyle topic.id model
            ++ style
        )
        children


effectiveDisplayMode : Id -> DisplayMode -> Model -> DisplayMode
effectiveDisplayMode topicId displayMode model =
    let
        isLimbo =
            model.search.menu == Open (Just topicId)
    in
    if isLimbo then
        case displayMode of
            Monad _ ->
                Monad Detail

            Container _ ->
                Container WhiteBox

    else
        displayMode


circleTopic : TopicInfo -> TopicProps -> MapPath -> Model -> TopicRendering
circleTopic topic props mapPath model =
    let
        mapId =
            getMapId mapPath

        mark =
            monadMark topic.text
    in
    ( topicPosStyle props
        ++ [ style "display" "flex"
           , style "align-items" "center"
           , style "justify-content" "center"
           , style "width" <| fromFloat topicSize.h ++ "px"
           , style "height" <| fromFloat topicSize.h ++ "px"
           , style "border-radius" "50%"
           , style "background-color" "white"
           ]
        ++ topicBorderStyle topic.id mapId model
        ++ selectionStyle topic.id mapId model
    , [ div
            [ style "font-family" "ui-monospace, SFMono-Regular, Menlo, Consolas, monospace"
            , style "font-size" "12px"
            , style "line-height" "1"
            , style "pointer-events" "none"
            , style "user-select" "none"
            , style "text-align" "center"
            ]
            [ Html.text mark ]
      ]
    )


labelTopic : TopicInfo -> TopicProps -> MapPath -> Model -> TopicRendering
labelTopic topic props mapPath model =
    let
        mapId =
            getMapId mapPath
    in
    ( topicPosStyle props
        ++ topicFlexboxStyle topic props mapId model
        ++ selectionStyle topic.id mapId model
    , labelTopicHtml topic props mapId model
    )


labelTopicHtml : TopicInfo -> TopicProps -> MapId -> Model -> List (Html Msg)
labelTopicHtml topic props mapId model =
    let
        isEdit =
            model.editState == ItemEdit topic.id mapId

        textElem =
            if isEdit then
                input
                    ([ id <| "dmx-input-" ++ fromInt topic.id ++ "-" ++ fromInt mapId
                     , value topic.text
                     , onInput (Edit << OnTextInput)
                     , onBlur (Edit EditEnd)
                     , onEnterOrEsc (Edit EditEnd)
                     , stopPropagationOnMousedown NoOp
                     ]
                        ++ topicInputStyle
                    )
                    []

            else
                div
                    topicLabelStyle
                    [ Html.text <| getTopicLabel topic ]
    in
    [ div
        (topicIconBoxStyle props)
        [ viewTopicIcon topic.id model ]
    , textElem
    ]


detailTopic : TopicInfo -> TopicProps -> MapPath -> Model -> TopicRendering
detailTopic topic props mapPath model =
    let
        mapId =
            getMapId mapPath

        isEdit =
            model.editState == ItemEdit topic.id mapId

        textElem =
            if isEdit then
                textarea
                    ([ id <| "dmx-input-" ++ fromInt topic.id ++ "-" ++ fromInt mapId
                     , onInput (Edit << OnTextareaInput)
                     , onBlur (Edit EditEnd)
                     , onEsc (Edit EditEnd)
                     , stopPropagationOnMousedown NoOp
                     ]
                        ++ detailTextStyle topic.id mapId model
                        ++ detailTextEditStyle topic.id mapId model
                    )
                    [ Html.text topic.text ]

            else
                div
                    (detailTextStyle topic.id mapId model
                        ++ detailTextViewStyle
                    )
                    (multilineHtml topic.text)
    in
    ( detailTopicStyle props
    , [ div
            (topicIconBoxStyle props
                ++ detailTopicIconBoxStyle
                ++ selectionStyle topic.id mapId model
            )
            [ viewTopicIcon topic.id model ]
      , textElem
      ]
    )


detailTopicStyle : TopicProps -> List (Attribute Msg)
detailTopicStyle { pos } =
    [ style "display" "flex"
    , style "left" <| fromFloat (pos.x - topicW2) ++ "px"
    , style "top" <| fromFloat (pos.y - topicH2) ++ "px"
    ]


detailTextStyle : Id -> MapId -> Model -> List (Attribute Msg)
detailTextStyle topicId mapId model =
    let
        r =
            fromInt topicRadius ++ "px"
    in
    [ style "font-size" <| fromInt contentFontSize ++ "px"
    , style "width" <| fromFloat topicDetailMaxWidth ++ "px"
    , style "line-height" <| fromFloat topicLineHeight
    , style "padding" <| fromInt topicDetailPadding ++ "px"
    , style "border-radius" <| "0 " ++ r ++ " " ++ r ++ " " ++ r
    ]
        ++ topicBorderStyle topicId mapId model
        ++ selectionStyle topicId mapId model


detailTextViewStyle : List (Attribute Msg)
detailTextViewStyle =
    [ style "min-width" <| fromFloat (topicSize.w - topicSize.h) ++ "px"
    , style "max-width" "max-content"
    , style "white-space" "pre-wrap"
    , style "pointer-events" "none"
    ]


detailTextEditStyle : Id -> MapId -> Model -> List (Attribute Msg)
detailTextEditStyle topicId mapId model =
    let
        height =
            case getTopicSize topicId mapId model.maps of
                Just size ->
                    size.h

                Nothing ->
                    0
    in
    [ style "position" "relative"
    , style "top" <| fromFloat -topicBorderWidth ++ "px"
    , style "height" <| fromFloat height ++ "px"
    , style "font-family" mainFont -- <textarea> default is "monospace"
    , style "border-color" "black" -- <textarea> default is some lightgray
    , style "resize" "none"
    ]


blackBoxTopic : TopicInfo -> TopicProps -> MapPath -> Model -> TopicRendering
blackBoxTopic topic props mapPath model =
    let
        mapId =
            getMapId mapPath
    in
    ( topicPosStyle props
    , [ div
            (topicFlexboxStyle topic props mapId model
                ++ blackBoxStyle
            )
            (labelTopicHtml topic props mapId model
                ++ mapItemCount topic.id props model
            )
      , div
            (ghostTopicStyle topic mapId model)
            []
      ]
    )


whiteBoxTopic : TopicInfo -> TopicProps -> MapPath -> Model -> TopicRendering
whiteBoxTopic topic props mapPath model =
    let
        ( style, children ) =
            labelTopic topic props mapPath model
    in
    ( style
    , children
        ++ mapItemCount topic.id props model
        ++ [ viewMap topic.id mapPath model ]
    )


unboxedTopic : TopicInfo -> TopicProps -> MapPath -> Model -> TopicRendering
unboxedTopic topic props mapPath model =
    let
        ( style, children ) =
            labelTopic topic props mapPath model
    in
    ( style
    , children
        ++ mapItemCount topic.id props model
    )


mapItemCount : Id -> TopicProps -> Model -> List (Html Msg)
mapItemCount topicId props model =
    let
        itemCount =
            case props.displayMode of
                Monad _ ->
                    0

                Container _ ->
                    case getMap topicId model.maps of
                        Just map ->
                            map.items |> Dict.values |> List.filter isVisible |> List.length

                        Nothing ->
                            0
    in
    [ div
        itemCountStyle
        [ Html.text <| fromInt itemCount ]
    ]


topicAttr : Id -> MapPath -> Model -> List (Attribute Msg)
topicAttr topicId mapPath model =
    if isFullscreen topicId model then
        []
        -- TODO: the fullscreen map would require dedicated event handling, e.g. panning?

    else
        [ attribute "class" "dmx-topic"
        , attribute "data-id" (fromInt topicId)
        , attribute "data-path" (fromPath mapPath)
        ]


viewAssoc : AssocInfo -> MapId -> Model -> Svg Msg
viewAssoc assoc mapId model =
    let
        geom =
            assocGeometry assoc mapId model
    in
    case geom of
        Just ( pos1, pos2 ) ->
            lineFunc (Just assoc) pos1 pos2

        Nothing ->
            Html.text ""



-- TODO


assocGeometry : AssocInfo -> MapId -> Model -> Maybe ( Point, Point )
assocGeometry assoc mapId model =
    let
        pos1 =
            getTopicPos assoc.player1 mapId model.maps

        pos2 =
            getTopicPos assoc.player2 mapId model.maps
    in
    case Maybe.map2 (\p1 p2 -> ( p1, p2 )) pos1 pos2 of
        Just geometry ->
            Just geometry

        Nothing ->
            fail "assocGeometry" { assoc = assoc, mapId = mapId } Nothing


viewLimboAssoc : MapId -> Model -> List (Svg Msg)
viewLimboAssoc mapId model =
    case model.mouse.dragState of
        Drag DrawAssoc _ mapPath origPos pos _ ->
            if getMapId mapPath == mapId then
                [ lineFunc Nothing origPos (relPos pos mapPath model) ]

            else
                []

        _ ->
            []


{-| Transforms an absolute screen position to a map-relative position.
-}
relPos : Point -> MapPath -> Model -> Point
relPos pos mapPath model =
    let
        posAbs =
            absMapPos mapPath (Point 0 0) model
    in
    Point
        (pos.x - posAbs.x)
        (pos.y - posAbs.y)


{-| Recursively calculates the absolute position of a map.
"posAcc" is the position accumulated so far.
-}
absMapPos : MapPath -> Point -> Model -> Point
absMapPos mapPath posAcc model =
    case mapPath of
        [ mapId ] ->
            accumulateMapRect posAcc mapId model

        mapId :: parentMapId :: mapIds ->
            accumulateMapPos posAcc mapId parentMapId mapIds model

        [] ->
            logError "absMapPos" "mapPath is empty!" (Point 0 0)


accumulateMapPos : Point -> MapId -> MapId -> MapPath -> Model -> Point
accumulateMapPos posAcc mapId parentMapId mapIds model =
    let
        { x, y } =
            accumulateMapRect posAcc mapId model
    in
    case getTopicPos mapId parentMapId model.maps of
        Just mapPos ->
            absMapPos
                -- recursion
                (parentMapId :: mapIds)
                (Point
                    (x + mapPos.x - topicW2)
                    (y + mapPos.y + topicH2)
                )
                model

        Nothing ->
            Point 0 0



-- error is already logged


accumulateMapRect : Point -> MapId -> Model -> Point
accumulateMapRect posAcc mapId model =
    case getMap mapId model.maps of
        Just map ->
            Point
                (posAcc.x - map.rect.x1)
                (posAcc.y - map.rect.y1)

        Nothing ->
            Point 0 0



-- error is already logged
-- STYLE


topicStyle : Id -> Model -> List (Attribute Msg)
topicStyle id model =
    let
        isLimbo =
            model.search.menu == Open (Just id)

        isDragging =
            case model.mouse.dragState of
                Drag DragTopic id_ _ _ _ _ ->
                    id_ == id

                _ ->
                    False
    in
    [ style "position" "absolute"
    , style "opacity" <|
        if isLimbo then
            ".5"

        else
            "1"
    , style "z-index" <|
        if isDragging then
            "1"

        else
            "2"
    ]


selectionStyle : Id -> MapId -> Model -> List (Attribute Msg)
selectionStyle topicId mapId model =
    if isSelected topicId mapId model then
        [ style "box-shadow" "gray 5px 5px 5px" ]

    else
        []


topicFlexboxStyle : TopicInfo -> TopicProps -> MapId -> Model -> List (Attribute Msg)
topicFlexboxStyle topic props mapId model =
    let
        r12 =
            fromInt topicRadius ++ "px"

        r34 =
            case props.displayMode of
                Container WhiteBox ->
                    "0"

                _ ->
                    r12
    in
    [ style "display" "flex"
    , style "align-items" "center"
    , style "gap" "8px"
    , style "width" <| fromFloat topicSize.w ++ "px"
    , style "height" <| fromFloat topicSize.h ++ "px"
    , style "border-radius" <| r12 ++ " " ++ r12 ++ " " ++ r34 ++ " " ++ r34
    ]
        ++ topicBorderStyle topic.id mapId model


topicPosStyle : TopicProps -> List (Attribute Msg)
topicPosStyle { pos } =
    [ style "left" <| fromFloat (pos.x - topicW2) ++ "px"
    , style "top" <| fromFloat (pos.y - topicH2) ++ "px"
    ]


topicIconBoxStyle : TopicProps -> List (Attribute Msg)
topicIconBoxStyle props =
    let
        r1 =
            fromInt topicRadius ++ "px"

        r4 =
            case props.displayMode of
                Container WhiteBox ->
                    "0"

                _ ->
                    r1
    in
    [ style "flex" "none"
    , style "width" <| fromFloat topicSize.h ++ "px"
    , style "height" <| fromFloat topicSize.h ++ "px"
    , style "border-radius" <| r1 ++ " 0 0 " ++ r4
    , style "background-color" "black"
    , style "pointer-events" "none"
    ]


detailTopicIconBoxStyle : List (Attribute Msg)
detailTopicIconBoxStyle =
    -- icon box correction as detail topic has no border, in contrast to label topic
    [ style "padding-left" <| fromFloat topicBorderWidth ++ "px"
    , style "width" <| fromFloat (topicSize.h - topicBorderWidth) ++ "px"
    ]


topicLabelStyle : List (Attribute Msg)
topicLabelStyle =
    [ style "font-size" <| fromInt contentFontSize ++ "px"
    , style "font-weight" topicLabelWeight
    , style "overflow" "hidden"
    , style "text-overflow" "ellipsis"
    , style "white-space" "nowrap"
    , style "pointer-events" "none"
    ]


topicInputStyle : List (Attribute Msg)
topicInputStyle =
    [ style "font-family" mainFont -- Default for <input> is "-apple-system" (on Mac)
    , style "font-size" <| fromInt contentFontSize ++ "px"
    , style "font-weight" topicLabelWeight
    , style "width" "100%"
    , style "position" "relative"
    , style "left" "-4px"
    , style "pointer-events" "initial"
    ]


blackBoxStyle : List (Attribute Msg)
blackBoxStyle =
    [ style "pointer-events" "none" ]


ghostTopicStyle : TopicInfo -> MapId -> Model -> List (Attribute Msg)
ghostTopicStyle topic mapId model =
    [ style "position" "absolute"
    , style "left" <| fromInt blackBoxOffset ++ "px"
    , style "top" <| fromInt blackBoxOffset ++ "px"
    , style "width" <| fromFloat topicSize.w ++ "px"
    , style "height" <| fromFloat topicSize.h ++ "px"
    , style "border-radius" <| fromInt topicRadius ++ "px"
    , style "pointer-events" "none"
    , style "z-index" "-1" -- behind topic
    ]
        ++ topicBorderStyle topic.id mapId model
        ++ selectionStyle topic.id mapId model


itemCountStyle : List (Attribute Msg)
itemCountStyle =
    [ style "font-size" <| fromInt contentFontSize ++ "px"
    , style "position" "absolute"
    , style "left" "calc(100% + 12px)"
    ]


whiteBoxStyle : Id -> Rectangle -> MapId -> Model -> List (Attribute Msg)
whiteBoxStyle topicId rect mapId model =
    let
        width =
            rect.x2 - rect.x1

        height =
            rect.y2 - rect.y1

        r =
            fromInt whiteBoxRadius ++ "px"
    in
    [ style "position" "absolute"
    , style "left" <| fromFloat -topicBorderWidth ++ "px"
    , style "top" <| fromFloat (topicSize.h - 2 * topicBorderWidth) ++ "px"
    , style "width" <| fromFloat width ++ "px"
    , style "height" <| fromFloat height ++ "px"
    , style "border-radius" <| "0 " ++ r ++ " " ++ r ++ " " ++ r
    ]
        ++ topicBorderStyle topicId mapId model
        ++ selectionStyle topicId mapId model


topicBorderStyle : Id -> MapId -> Model -> List (Attribute Msg)
topicBorderStyle id mapId model =
    let
        targeted =
            case model.mouse.dragState of
                -- can't move a topic to a map where it is already
                -- can't create assoc when both topics are in different map
                Drag DragTopic _ (mapId_ :: _) _ _ target ->
                    isTarget id mapId target && mapId_ /= id

                Drag DrawAssoc _ (mapId_ :: _) _ _ target ->
                    isTarget id mapId target && mapId_ == mapId

                _ ->
                    False
    in
    [ style "border-width" <| fromFloat topicBorderWidth ++ "px"
    , style "border-style" <|
        if targeted then
            "dashed"

        else
            "solid"
    , style "box-sizing" "border-box"
    , style "background-color" "white"
    ]


isTarget : Id -> MapId -> Maybe ( Id, MapPath ) -> Bool
isTarget topicId mapId target =
    case target of
        Just ( targetId, targetMapPath ) ->
            case targetMapPath of
                targetMapId :: _ ->
                    topicId == targetId && mapId == targetMapId

                [] ->
                    False

        Nothing ->
            False


topicLayerStyle : Rectangle -> List (Attribute Msg)
topicLayerStyle mapRect =
    [ style "position" "absolute"
    , style "left" <| fromFloat -mapRect.x1 ++ "px"
    , style "top" <| fromFloat -mapRect.y1 ++ "px"
    ]


svgStyle : List (Attribute Msg)
svgStyle =
    [ style "position" "absolute" -- occupy entire window height (instead 150px default height)
    , style "top" "0"
    , style "left" "0"
    ]



-- One possible line func
-- One possible line func


taxiLine : Maybe AssocInfo -> Point -> Point -> Svg Msg
taxiLine assoc pos1 pos2 =
    if abs (pos2.x - pos1.x) < 2 * assocRadius then
        -- straight vertical
        let
            xm =
                (pos1.x + pos2.x) / 2
        in
        path
            (d ("M " ++ fromFloat xm ++ " " ++ fromFloat pos1.y ++ " V " ++ fromFloat pos2.y)
                :: lineStyle assoc
            )
            []

    else if abs (pos2.y - pos1.y) < 2 * assocRadius then
        -- straight horizontal
        let
            ym =
                (pos1.y + pos2.y) / 2
        in
        path
            (d ("M " ++ fromFloat pos1.x ++ " " ++ fromFloat ym ++ " H " ++ fromFloat pos2.x)
                :: lineStyle assoc
            )
            []

    else
        -- 5 segment taxi line
        let
            sx =
                if pos2.x > pos1.x then
                    1

                else
                    -1

            -- sign x
            sy =
                if pos2.y > pos1.y then
                    -1

                else
                    1

            -- sign y
            ym =
                (pos1.y + pos2.y) / 2

            -- y mean
            x1 =
                fromFloat (pos1.x + sx * assocRadius)

            x2 =
                fromFloat (pos2.x - sx * assocRadius)

            y1 =
                fromFloat (ym + sy * assocRadius)

            y2 =
                fromFloat (ym - sy * assocRadius)

            sweep1 =
                if sy == 1 then
                    if sx == 1 then
                        1

                    else
                        0

                else if sx == 1 then
                    0

                else
                    1

            sweep2 =
                1 - sweep1

            sw1 =
                fromInt sweep1

            sw2 =
                fromInt sweep2

            r =
                fromFloat assocRadius
        in
        path
            (d
                ("M "
                    ++ fromFloat pos1.x
                    ++ " "
                    ++ fromFloat pos1.y
                    ++ " V "
                    ++ y1
                    ++ " A "
                    ++ r
                    ++ " "
                    ++ r
                    ++ " 0 0 "
                    ++ sw1
                    ++ " "
                    ++ x1
                    ++ " "
                    ++ fromFloat ym
                    ++ " H "
                    ++ x2
                    ++ " A "
                    ++ r
                    ++ " "
                    ++ r
                    ++ " 0 0 "
                    ++ sw2
                    ++ " "
                    ++ fromFloat pos2.x
                    ++ " "
                    ++ y2
                    ++ " V "
                    ++ fromFloat pos2.y
                )
                :: lineStyle assoc
            )
            []


lineStyle : Maybe AssocInfo -> List (Attribute Msg)
lineStyle assoc =
    [ stroke assocColor
    , strokeWidth <| fromFloat assocWidth ++ "px"
    , strokeDasharray <| lineDasharray assoc
    , fill "none"
    ]


lineDasharray : Maybe AssocInfo -> String
lineDasharray maybeAssoc =
    case maybeAssoc of
        Just { itemType } ->
            case itemType of
                "dmx.association" ->
                    "5 0"

                -- solid
                "dmx.composition" ->
                    "5"

                -- dotted
                _ ->
                    "1"

        -- error
        Nothing ->
            "5 0"


{-| Pick a short “mark” for the monad interior.
Strategy:

  - prefer the first non-space “word” up to 3 chars
  - else first 2 visible chars
  - else "•"

-}
monadMark : String -> String
monadMark title =
    let
        trimmed =
            String.trim title

        word =
            trimmed
                |> String.words
                |> List.head
                |> Maybe.withDefault trimmed

        take n s =
            String.left n s
    in
    if String.isEmpty trimmed then
        "•"

    else if String.length word >= 1 then
        word |> take (min 3 (String.length word))

    else
        take (min 2 (String.length trimmed)) trimmed
