module MapRenderer exposing (viewMap)

import Config exposing (..)
import IconMenu exposing (viewTopicIcon)
import Model exposing (..)
import Utils exposing (..)

import Dict
import Html exposing (Html, Attribute, div, text, input, textarea)
import Html.Attributes exposing (id, style, attribute, value)
import Html.Events exposing (onInput, onBlur)
import String exposing (fromInt, fromFloat)
import Svg exposing (Svg, svg, line, path)
import Svg.Attributes exposing (width, height, viewBox, x1, y1, x2, y2, d, stroke, strokeWidth,
  strokeDasharray, fill)



-- CONFIG


lineFunc = taxiLine -- directLine



-- MODEL


type alias MapInfo =
  ( ( List (Html Msg), List (Svg Msg) )
  , Rectangle
  , ( { w: String, h: String }
    , List (Attribute Msg)
    , List (Attribute Msg)
    )
  )

type alias TopicRendering = (List (Attribute Msg), List (Html Msg))



-- VIEW


viewMap : MapId -> MapId -> Model -> Html Msg
viewMap mapId parentMapId model =
  let
    ((topics, assocs), mapRect, (svgSize, svgAttr, mapStyle)) = mapInfo mapId parentMapId model
  in
  div
    mapStyle
    [ div
        (topicLayerStyle mapRect)
        topics
    , svg
        ( [ width svgSize.w, height svgSize.h ]
          ++ topicAttr mapId parentMapId
          ++ svgAttr
          ++ svgStyle
        )
        ( assocs
          ++ viewLimboAssoc mapId model
        )
    ]


mapInfo : MapId -> MapId -> Model -> MapInfo
mapInfo mapId parentMapId model =
  let
    isTopLevel = mapId == model.activeMap
  in
  case getMap mapId model.maps of
    Just map ->
      ( viewItems map model
      , map.rect
      , if isTopLevel then
          ( {w = "100%", h = "100%"}, [], [] )
        else
          let
            vb =
              { x = map.rect.x1 |> round |> fromInt
              , y = map.rect.y1 |> round |> fromInt
              , w = (map.rect.x2 - map.rect.x1) |> round |> fromInt
              , h = (map.rect.y2 - map.rect.y1) |> round |> fromInt
              }
          in
          ( {w = vb.w, h = vb.h}
          , [ viewBox (vb.x ++ " " ++ vb.y ++ " " ++ vb.w ++ " " ++ vb.h) ]
          , whiteBoxStyle mapId map.rect parentMapId model
          )
      )
    Nothing ->
      ( ([], []), Rectangle 0 0 0 0, ( {w = "0", h = "0"}, [], [] ))


viewItems : Map -> Model -> (List (Html Msg), List (Svg Msg))
viewItems map model =
  map.items |> Dict.values |> List.filter isVisible |> List.foldr
    (\{id, viewProps} (t, a) ->
      let
        item = model.items |> Dict.get id
      in
      case (item, viewProps) of
        (Just (Topic topic), ViewTopic props) -> (viewTopic topic props map.id model :: t, a)
        (Just (Assoc assoc), ViewAssoc _) -> (t, viewAssoc assoc map.id model :: a)
        _ -> logError "viewItems" ("problem with item " ++ fromInt id) (t, a)
    )
    ([], [])


viewTopic : TopicInfo -> TopicProps -> MapId -> Model -> Html Msg
viewTopic topic props mapId model =
  let
    (style, children) = topicFunc topic props mapId model
    topicFunc =
      case props.displayMode of
        Monad LabelOnly -> labelTopic
        Monad Detail -> detailTopic
        Container BlackBox -> blackBoxTopic
        Container WhiteBox -> whiteBoxTopic
        Container Unboxed -> unboxedTopic
  in
  div
    ( topicAttr topic.id mapId
      ++ topicStyle topic mapId model
      ++ style
    )
    children


labelTopic : TopicInfo -> TopicProps -> MapId -> Model -> TopicRendering
labelTopic topic props mapId model =
  ( topicPosStyle props
      ++ topicFlexboxStyle topic props mapId model
      ++ selectionStyle topic.id mapId model
  , labelTopicHtml topic props mapId model
  )


labelTopicHtml : TopicInfo -> TopicProps -> MapId -> Model -> List (Html Msg)
labelTopicHtml topic props mapId model =
  let
    isEdit = model.editState == ItemEdit topic.id mapId
    textElem =
      if isEdit then
        input
          ( [ id <| "dmx-input-" ++ fromInt topic.id ++ "-" ++ fromInt mapId
            , value topic.text
            , onInput (Edit << ItemEditInput)
            , onBlur (Edit ItemEditEnd)
            , onEnterOrEsc (Edit ItemEditEnd)
            , stopPropagationOnMousedown NoOp
            ]
            ++ topicInputStyle
          )
          []
      else
        let
          label = case topic.text |> String.lines |> List.head of
            Just line -> line
            Nothing -> ""
        in
        div
          topicLabelStyle
          [ text label ]
  in
  [ div
    (topicIconBoxStyle props)
    [ viewTopicIcon topic.id model ]
  , textElem
  ]


detailTopic : TopicInfo -> TopicProps -> MapId -> Model -> TopicRendering
detailTopic topic props mapId model =
  let
    isEdit = model.editState == ItemEdit topic.id mapId
    textElem =
      if isEdit then
        textarea
          ( [ id <| "dmx-input-" ++ fromInt topic.id ++ "-" ++ fromInt mapId
            , onInput (Edit << TextareaInput)
            , onBlur (Edit ItemEditEnd)
            , onEsc (Edit ItemEditEnd)
            , stopPropagationOnMousedown NoOp
            ]
            ++ detailTextStyle topic.id mapId model
            ++ detailTextEditStyle topic.id mapId model
          )
          [ text topic.text ]
      else
        div
          ( detailTextStyle topic.id mapId model
            ++ detailTextViewStyle
          )
          ( multilineHtml topic.text )
  in
  ( detailTopicStyle props
  , [ div
      ( topicIconBoxStyle props
        ++ detailTopicIconBoxStyle
        ++ selectionStyle topic.id mapId model
      )
      [ viewTopicIcon topic.id model ]
    , textElem
    ]
  )


detailTopicStyle : TopicProps -> List (Attribute Msg)
detailTopicStyle {pos} =
  [ style "display" "flex"
  , style "left" <| fromFloat (pos.x - topicW2) ++ "px"
  , style "top" <| fromFloat (pos.y - topicH2) ++ "px"
  ]


detailTextStyle : Id -> MapId -> Model -> List (Attribute Msg)
detailTextStyle topicId mapId model =
  let
    r = fromInt topicRadius ++ "px"
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
    height = case getTopicSize topicId mapId model.maps of
      Just size -> size.h
      Nothing -> 0
  in
  [ style "position" "relative"
  , style "top" <| fromFloat -topicBorderWidth ++ "px"
  , style "height" <| fromFloat height ++ "px"
  , style "font-family" mainFont -- <textarea> default is "monospace"
  , style "border-color" "black" -- <textarea> default is some lightgray
  , style "resize" "none"
  ]


blackBoxTopic : TopicInfo -> TopicProps -> MapId -> Model -> TopicRendering
blackBoxTopic topic props mapId model =
  ( topicPosStyle props
  , [ div
      (topicFlexboxStyle topic props mapId model ++ blackBoxStyle)
      (labelTopicHtml topic props mapId model ++ viewItemCount topic.id props model)
    , div
      (ghostTopicStyle topic mapId model)
      []
    ]
  )


whiteBoxTopic : TopicInfo -> TopicProps -> MapId -> Model -> TopicRendering
whiteBoxTopic topic props mapId model =
  let
    (style, children) = labelTopic topic props mapId model
  in
  ( style
  , children
    ++ viewItemCount topic.id props model
    ++ [ viewMap topic.id mapId model ]
  )


unboxedTopic : TopicInfo -> TopicProps -> MapId -> Model -> TopicRendering
unboxedTopic topic props mapId model =
  let
    (style, children) = labelTopic topic props mapId model
  in
  ( style
  , children
    ++ viewItemCount topic.id props model
  )


viewItemCount : Id -> TopicProps -> Model -> List (Html Msg)
viewItemCount topicId props model =
  let
    itemCount =
      case props.displayMode of
        Monad _ -> 0
        Container _ ->
          case getMap topicId model.maps of
            Just map -> map.items |> Dict.values |> List.filter isVisible |> List.length
            Nothing -> 0
  in
  [ div
      itemCountStyle
      [ text <| fromInt itemCount ]
  ]


topicAttr : Id -> MapId -> List (Attribute Msg)
topicAttr id mapId =
  case id of
    0 -> [] -- top-level map requires dedicated event handling, TODO
    _ ->
      [ attribute "class" "dmx-topic"
      , attribute "data-id" (fromInt id)
      , attribute "data-map-id" (fromInt mapId)
      ]


viewAssoc : AssocInfo -> MapId -> Model -> Svg Msg
viewAssoc assoc mapId model =
  let
    geom = assocGeometry assoc mapId model
  in
  case geom of
    Just (pos1, pos2) -> lineFunc (Just assoc) pos1 pos2
    Nothing -> text "" -- TODO


viewLimboAssoc : MapId -> Model -> List (Svg Msg)
viewLimboAssoc mapId model =
  case model.dragState of
    Drag DrawAssoc topicId mapId_ _ pos _ ->
      if mapId_ == mapId then
        let
          points = Maybe.map2
            (\pos1 pos2 -> (pos1, pos2))
            (getTopicPos topicId mapId model.maps)
            (relPos pos mapId model)
        in
        case points of
          Just (pos1, pos2) -> [ lineFunc Nothing pos1 pos2 ]
          Nothing -> []
      else
        []
    _ -> []


assocGeometry : AssocInfo -> MapId -> Model -> Maybe (Point, Point)
assocGeometry assoc mapId model =
  let
    pos1 = getTopicPos assoc.player1 mapId model.maps
    pos2 = getTopicPos assoc.player2 mapId model.maps
  in
  case Maybe.map2 (\p1 p2 -> ( p1, p2 )) pos1 pos2 of
    Just geometry -> Just geometry
    Nothing -> fail "assocGeometry" { assoc = assoc, mapId = mapId } Nothing


{-| Transforms an absolute screen position to a map-relative position.
-}
relPos : Point -> MapId -> Model -> Maybe Point
relPos pos mapId model =
  absMapPos mapId (Point 0 0) model |> Maybe.andThen
    (\posAbs -> Just
      (Point
        (pos.x - posAbs.x)
        (pos.y - posAbs.y)
      )
    )


{-| Recursively calculates the absolute position of a map.
    "posAcc" is the position accumulated so far.
-}
absMapPos : MapId -> Point -> Model -> Maybe Point
absMapPos mapId posAcc model =
  if mapId == model.activeMap then
    Just posAcc
  else
    getMap mapId model.maps
      |> Maybe.andThen
        (\map -> getTopicPos map.id map.parentMapId model.maps
          |> Maybe.andThen
            (\mapPos_ ->
              absMapPos
                map.parentMapId
                (Point
                  (posAcc.x + mapPos_.x - topicW2 - map.rect.x1)
                  (posAcc.y + mapPos_.y + topicH2 - map.rect.y1)
                )
                model
            )
        )



-- STYLE


topicStyle : TopicInfo -> MapId -> Model -> List (Attribute Msg)
topicStyle ({ id } as topic) mapId model =
  let
    dragging = case model.dragState of
      Drag DragTopic id_ _ _ _ _ -> id_ == id
      _ -> False
  in
  [ style "position" "absolute"
  , style "z-index" <| if dragging then "1" else "2"
  ]


selectionStyle : Id -> MapId -> Model -> List (Attribute Msg)
selectionStyle topicId mapId model =
  let
    selected = model.selection |> List.member (topicId, mapId)
  in
  if selected then
    [ style "box-shadow" "gray 5px 5px 5px" ]
  else
    []


topicFlexboxStyle : TopicInfo -> TopicProps -> MapId -> Model -> List (Attribute Msg)
topicFlexboxStyle topic props mapId model =
  let
    r12 = fromInt topicRadius ++ "px"
    r34 = case props.displayMode of
      Container WhiteBox -> "0"
      _ -> r12
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
    r1 = fromInt topicRadius ++ "px"
    r4 = case props.displayMode of
      Container WhiteBox -> "0"
      _ -> r1
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
  [ style "position" "absolute"
  , style "left" "calc(100% + 14px)"
  ]


whiteBoxStyle : Id -> Rectangle -> MapId -> Model -> List (Attribute Msg)
whiteBoxStyle topicId rect mapId model =
  let
    width = rect.x2 - rect.x1
    height = rect.y2 - rect.y1
    r = fromInt whiteBoxRadius ++ "px"
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
    targeted = case model.dragState of
      -- can't move a topic to a map where it is already
      -- can't create assoc when both topics are in different map
      Drag DragTopic _ mapId_ _ _ (Just target) -> target == (id, mapId) && mapId_ /= id
      Drag DrawAssoc _ mapId_ _ _ (Just target) -> target == (id, mapId) && mapId_ == mapId
      _ -> False
  in
  [ style "border-width" <| fromFloat topicBorderWidth ++ "px"
  , style "border-style" <| if targeted then "dashed" else "solid"
  , style "box-sizing" "border-box"
  , style "background-color" "white"
  ]


topicLayerStyle : Rectangle -> List (Attribute Msg)
topicLayerStyle mapRect =
  [ style "position" "absolute"
  , style "left" <| fromFloat -mapRect.x1 ++ "px"
  , style "top" <| fromFloat -mapRect.y1 ++ "px"
  ]


svgStyle : List (Attribute Msg)
svgStyle =
  [ style "position" "absolute"
  , style "top" "0"
  , style "left" "0"
  ]


-- One possible line func
directLine : Maybe AssocInfo -> Point -> Point -> Svg Msg
directLine assoc pos1 pos2 =
  line
    ( [ x1 <| fromFloat pos1.x
      , y1 <| fromFloat pos1.y
      , x2 <| fromFloat pos2.x
      , y2 <| fromFloat pos2.y
      ] ++ lineStyle assoc
    )
    []


-- One possible line func
taxiLine : Maybe AssocInfo -> Point -> Point -> Svg Msg
taxiLine assoc pos1 pos2 =
  if abs (pos2.x - pos1.x) < 2 * assocRadius then -- straight vertical
    let
      xm = (pos1.x + pos2.x) / 2
    in
    path
      ( [ d ("M " ++ fromFloat xm ++ " " ++ fromFloat pos1.y ++ " V " ++ fromFloat pos2.y)
        ] ++ lineStyle assoc
      )
      []
  else if abs (pos2.y - pos1.y) < 2 * assocRadius then -- straight horizontal
    let
      ym = (pos1.y + pos2.y) / 2
    in
    path
      ( [ d ("M " ++ fromFloat pos1.x ++ " " ++ fromFloat ym ++ " H " ++ fromFloat pos2.x)
        ] ++ lineStyle assoc
      )
      []
  else -- 5 segment taxi line
    let
      sx = if pos2.x > pos1.x then 1 else -1 -- sign x
      sy = if pos2.y > pos1.y then -1 else 1 -- sign y
      ym = (pos1.y + pos2.y) / 2 -- y mean
      x1 = fromFloat (pos1.x + sx * assocRadius)
      x2 = fromFloat (pos2.x - sx * assocRadius)
      y1 = fromFloat (ym + sy * assocRadius)
      y2 = fromFloat (ym - sy * assocRadius)
      sweep1 =
        if sy == 1 then
          if sx == 1 then 1 else 0
        else
          if sx == 1 then 0 else 1
      sweep2 = 1 - sweep1
      sw1 = fromInt sweep1
      sw2 = fromInt sweep2
      r = fromFloat assocRadius
    in
    path
      ( [ d
          ( "M " ++ fromFloat pos1.x ++ " " ++ fromFloat pos1.y ++
            " V " ++ y1 ++
            " A " ++ r ++ " " ++ r ++ " 0 0 " ++ sw1 ++ " " ++ x1 ++ " " ++ fromFloat ym ++
            " H " ++ x2 ++
            " A " ++ r ++ " " ++ r ++ " 0 0 " ++ sw2 ++ " " ++ fromFloat pos2.x ++ " " ++ y2 ++
            " V " ++ fromFloat pos2.y
          )
        ] ++ lineStyle assoc
      )
      []


lineStyle : Maybe AssocInfo -> List (Attribute Msg)
lineStyle assoc =
  [ stroke assocColor
  , strokeWidth <| fromFloat assocWith ++ "px"
  , strokeDasharray <| lineDasharray assoc
  , fill "none"
  ]


lineDasharray : Maybe AssocInfo -> String
lineDasharray maybeAssoc =
  case maybeAssoc of
    Just { itemType } ->
      case itemType of
        "dmx.association" -> "5 0" -- solid
        "dmx.composition" -> "5" -- dotted
        _ -> "1" -- error
    Nothing -> "5 0" -- solid
