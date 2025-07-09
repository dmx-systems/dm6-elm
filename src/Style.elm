module Style exposing (..)

import Model exposing (..)
import Html exposing (Attribute)
import Html.Attributes exposing (style)
import String exposing (String, fromInt, fromFloat)
import Svg
import Svg.Attributes exposing (x1, y1, x2, y2, d, stroke, strokeWidth, fill)



-- CONFIG


mainFontSize = "14px"

assocWith = 1.5
assocRadius = 14
assocColor = "black"

topicSize = { w = 128, h = 28 }
topicIconSize = 18
topicBorderWidth = 1
topicRadius = 7
topicRect =
  Rectangle
    (-topicSize.w / 2) (-topicSize.h / 2)
    (topicSize.w / 2) (topicSize.h / 2)

blackBoxSize = 42
blackBoxOffset = blackBoxSize / 2
blackBoxRadius = 10
blackBoxRect =
  Rectangle
    -blackBoxOffset -blackBoxOffset
    blackBoxOffset blackBoxOffset

whiteboxRange = { width = 250, height = 150 }
whiteboxRadius = 20
whiteboxPadding = 12



-- STYLE


appStyle : List (Attribute Msg)
appStyle =
  [ style "font-family" "sans-serif"
  , style "font-size" mainFontSize
  , style "user-select" "none"
  , style "-webkit-user-select" "none" -- Safari still needs vendor prefix
  ]


toolbarStyle : List (Attribute Msg)
toolbarStyle =
  [ style "display" "inline-flex"
  , style "flex-direction" "column"
  , style "align-items" "flex-start"
  , style "gap" "28px"
  , style "margin-top" "20px"
  ]


displayModeStyle : Bool -> List (Attribute Msg)
displayModeStyle disabled =
  let
    (color, pointerEvents) =
      if disabled then
        ("gray", "none")
      else
        ("unset", "unset")
  in
  [ style "display" "flex"
  , style "flex-direction" "column"
  , style "gap" "6px"
  , style "color" color
  , style "pointer-events" pointerEvents
  ]


buttonStyle : List (Attribute Msg)
buttonStyle =
  [ style "font-family" "sans-serif"
  , style "font-size" mainFontSize
  ]


topicStyle : TopicInfo -> MapId -> Model -> List (Attribute Msg)
topicStyle { id } mapId model =
  let
    selected = model.selection |> List.member (id, mapId)
    dragging = case model.dragState of
      Drag DragTopic id_ _ _ _ _ -> id_ == id
      _ -> False
    targeted = case model.dragState of
      -- can't move a topic to a map where it is already
      Drag DragTopic _ mapId_ _ _ (Just target) -> target == (id, mapId) && mapId_ /= id
      -- can't create assoc when both topics are in different map
      Drag DrawAssoc _ mapId_ _ _ (Just target) -> target == (id, mapId) && mapId_ == mapId
      _ -> False
  in
  [ style "position" "absolute"
  , style "border-width" <| fromFloat topicBorderWidth ++ "px"
  , style "border-style" <| if targeted then "dashed" else "solid"
  , style "box-sizing" "border-box"
  , style "background-color" "white"
  , style "z-index" <| if dragging then "0" else "1"
  ]
  ++ if selected then selectionStyle else []


selectionStyle : List (Attribute Msg)
selectionStyle =
  [ style "box-shadow" "4px 4px 4px gray" ]


normalStyle : TopicInfo -> TopicProps -> List (Attribute Msg)
normalStyle { color } { pos } =
  [ style "left" <| fromFloat (pos.x - topicSize.w / 2) ++ "px"
  , style "top" <| fromFloat (pos.y - topicSize.h / 2) ++ "px"
  , style "width" <| fromInt topicSize.w ++ "px"
  , style "height" <| fromInt topicSize.h ++ "px"
  , style "border-radius" <| fromInt topicRadius ++ "px"
  ]


blackBoxStyle : TopicInfo -> TopicProps -> List (Attribute Msg)
blackBoxStyle { color } { pos } =
  [ style "left" <| fromFloat (pos.x - blackBoxOffset) ++ "px"
  , style "top" <| fromFloat (pos.y - blackBoxOffset) ++ "px"
  , style "width" <| fromInt blackBoxSize ++ "px"
  , style "height" <| fromInt blackBoxSize ++ "px"
  , style "border-radius" <| fromInt blackBoxRadius ++ "px"
  ]


whiteboxStyle : TopicInfo -> TopicProps -> Rectangle -> List (Attribute Msg)
whiteboxStyle { color } { pos } rect =
  let
    width = rect.x2 - rect.x1
    height = rect.y2 - rect.y1
  in
  [ style "left" <| fromFloat (pos.x - width / 2) ++ "px"
  , style "top" <| fromFloat (pos.y - height / 2) ++ "px"
  , style "width" <| fromFloat width ++ "px"
  , style "height" <| fromFloat height ++ "px"
  , style "border-radius" <| fromInt whiteboxRadius ++ "px"
  ]


itemCountStyle : List (Attribute Msg)
itemCountStyle =
  [ style "position" "relative"
  , style "top" "-1.3em"
  , style "text-align" "center"
  ]


nestedMapStyle : List (Attribute Msg)
nestedMapStyle =
  [ style "position" "absolute"
  , style "left" <| fromFloat -topicBorderWidth ++ "px"
  , style "top" <| fromFloat -topicBorderWidth ++ "px"
  , style "width" <| "calc(100% + " ++ fromFloat (2 * topicBorderWidth) ++ "px)"
  , style "height" <| "calc(100% + " ++ fromFloat (2 * topicBorderWidth) ++ "px)"
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
  , style "z-index" "-1" -- behind topic layer
  ]


lineStyle : Point -> Point -> List (Attribute Msg)
lineStyle pos1 pos2 =
  [ x1 <| fromFloat pos1.x
  , y1 <| fromFloat pos1.y
  , x2 <| fromFloat pos2.x
  , y2 <| fromFloat pos2.y
  , stroke assocColor
  , strokeWidth <| fromFloat assocWith ++ "px"
  ]


taxiLineStyle : Point -> Point -> List (Attribute Msg)
taxiLineStyle pos1 pos2 =
  let
    sx = if pos2.x > pos1.x then 1 else -1 -- sign x
    sy = if pos2.y > pos1.y then -1 else 1 -- sign y
    ym = (pos1.y + pos2.y) / 2
    x1 = pos1.x + sx * assocRadius
    x2 = pos2.x - sx * assocRadius
    y1 = ym + sy * assocRadius
    y2 = ym - sy * assocRadius
    sweep1 =
      if sy == 1 then
        if sx == 1 then 1 else 0
      else
        if sx == 1 then 0 else 1
    sweep2 = 1 - sweep1
  in
  [ d ( "M " ++ fromFloat pos1.x ++ " " ++ fromFloat pos1.y ++
        " V " ++ fromFloat y1 ++
        " A " ++ fromFloat assocRadius ++ " " ++ fromFloat assocRadius ++ " 0 0 " ++
        fromInt sweep1 ++ " " ++ fromFloat x1 ++ " " ++ fromFloat ym ++
        " H " ++ fromFloat x2 ++
        " A " ++ fromFloat assocRadius ++ " " ++ fromFloat assocRadius ++ " 0 0 " ++
        fromInt sweep2 ++ " " ++ fromFloat pos2.x ++ " " ++ fromFloat y2 ++
        " V " ++ fromFloat pos2.y
      )
  , stroke assocColor
  , strokeWidth <| fromFloat assocWith ++ "px"
  , fill "none"
  ]


-- Edit Dialog

editDialogStyle : List (Attribute Msg)
editDialogStyle =
  [ style "position" "absolute"
  , style "left" "72px"
  , style "top" "244px"
  , style "width" "320px"
  , style "height" "320px"
  , style "background-color" "white"
  , style "border" "1px solid lightgray"
  ]


iconsStyle : List (Attribute Msg)
iconsStyle =
  [ style "height" "100%"
  , style "overflow" "auto"
  ]


iconButtonStyle : List (Attribute Msg)
iconButtonStyle =
  [ --style "background-color" "white"
    style "border-width" "0"
  , style "margin" "8px"
  ]


closeButtonStyle : List (Attribute Msg)
closeButtonStyle =
  [ style "position" "absolute"
  , style "top" "0"
  , style "right" "0"
  ]


topicIconBoxStyle : List (Attribute Msg)
topicIconBoxStyle =
  [ style "position" "relative"
  , style "top" <| fromFloat -topicBorderWidth ++ "px"
  , style "left" <| fromFloat -topicBorderWidth ++ "px"
  , style "width" <| fromInt topicSize.h ++ "px"
  , style "height" <| fromInt topicSize.h ++ "px"
  , style "border-radius" <| fromInt topicRadius ++ "px 0 0 " ++ fromInt topicRadius ++ "px"
  , style "background-color" "black"
  ]


topicIconStyle : List (Attribute Msg)
topicIconStyle =
  [ style "position" "relative"
  , style "top" <| fromFloat ((topicSize.h - topicIconSize) / 2) ++ "px"
  , style "left" <| fromFloat ((topicSize.h - topicIconSize) / 2) ++ "px"
  , style "color" "white"
  , style "pointer-events" "none"
  ]
