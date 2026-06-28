module TopicMap.View exposing (view)

import Assoc
import Box
import Config as C
import Env exposing (ExtManager, Env2)
import Feature.Icon as Icon
import Feature.Mouse as Mouse
import Feature.Sel as Sel
import Feature.Text as Text
import Feature.ToolDef exposing (LineStyle(..))
import Feature.Tool as Tool exposing (ToolbarPos)
import Model exposing (Model, Msg(..))
import ModelBase exposing (..)
import Shared.Events as Events
import Shared.ViewBase as VB
import Topic
import TopicMap.TopicMap as TM
import TopicMap.TopicMapDef as TopicMapDef exposing (TopicMap, MapTopic, DragState(..),
  DragMode(..))
import TopicMap.ViewModel as VM
import Utils as U

import Html exposing (Html, div, text)
import Html.Attributes exposing (id, style)
import String exposing (fromInt, fromFloat)
import Svg exposing (Svg)
import Svg.Attributes exposing (width, height, x, y, x1, y1, x2, y2, d, stroke, fill, transform,
  strokeWidth, strokeDasharray, dx, dy, stdDeviation, floodColor, filter, filterUnits)



-- MODEL


type alias BoxInfo =
  ( ( List (Html Msg), List (Svg Msg) )
  , Rectangle
  , ( { w: String, h: String }
    , Attrs Msg
    )
  )


type alias TopicRendering =
  (Attrs Msg, List (Html Msg))


type alias LineRenderer =
  Point -> Point -> Maybe Assoc -> BoxPath -> Attrs Msg -> Model -> List (Svg Msg)



-- VIEW


-- ExtManager.ExtBoxView
-- For the fullscreen box boxPath is empty
view : BoxId -> BoxPath -> Env2 -> Html Msg
view boxId boxPath ({model} as env) =
  let
    boxPath_ = boxId :: boxPath
    ((topics, assocs), boxRect, (svgSize, boxStyle)) =
      boxInfo boxId boxPath env
  in
  div
    boxStyle
    ( [ div
          ( topicLayerStyle boxRect )
          topics
      , Svg.svg
          ( [ width svgSize.w, height svgSize.h ]
            ++ svgStyle
          )
          [ svgDefs
          , Svg.g
              ( gAttr boxId boxRect model )
              ( assocs
                ++ viewLimboAssoc boxId model
                ++ viewAssocDraft boxId model
              )
          ]
      ]
      ++ Tool.viewToolbar boxPath_ (toolbarPos boxPath_ model) env
    )


topicLayerStyle : Rectangle -> Attrs Msg
topicLayerStyle boxRect =
  [ style "position" "absolute"
  , style "left" <| fromInt -boxRect.x1 ++ "px"
  , style "top" <| fromInt -boxRect.y1 ++ "px"
  ]


svgStyle : Attrs Msg
svgStyle =
  [ style "position" "absolute" -- occupy entire window height (instead 150px default height)
  , style "top" "0"
  , style "left" "0"
  ]


svgDefs : Svg Msg
svgDefs =
  Svg.defs
    []
    [ Svg.filter
        [ id "shadow"
        , x "-100%"
        , y "-100%"
        , width "200%"
        , height "200%"
        , filterUnits "userSpaceOnUse"
        ]
        [ Svg.node "feDropShadow"
            [ dx <| fromInt C.assocDropShadow.dx
            , dy <| fromInt C.assocDropShadow.dy
            , stdDeviation <| fromInt C.assocDropShadow.stdDeviation
            , floodColor C.assocDropShadow.floodColor
            ]
            []
        ]
    ]


gAttr : BoxId -> Rectangle -> Model -> Attrs Msg
gAttr boxId boxRect model =
  [ transform
      <| "translate(" ++ fromInt -boxRect.x1 ++ " " ++ fromInt -boxRect.y1 ++ ")"
  ]


-- For the fullscreen box boxPath is empty
boxInfo : BoxId -> BoxPath -> Env2 -> BoxInfo
boxInfo boxId boxPath ({model} as env) =
  case TM.byId boxId model of
    Just topicMap ->
      let
        width = topicMap.rect.x2 - topicMap.rect.x1
        height = topicMap.rect.y2 - topicMap.rect.y1
      in
      ( viewItems topicMap boxPath env
      , topicMap.rect
      , ( { w = fromInt width
          , h = fromInt height
          }
        , VB.boxStyle boxId boxPath (Size width height) model
        )
      )
    Nothing ->
      U.fail "TopicMap.View.boxInfo" {boxId = boxId, boxPath = boxPath}
        ( ([], []), Rectangle 0 0 0 0, ( {w = "0", h = "0"}, [] ))


-- For the fullscreen box boxPath is empty
viewItems : TopicMap -> BoxPath -> Env2 -> (List (Html Msg), List (Svg Msg))
viewItems topicMap boxPath ({model} as env) =
  let
    newPath = topicMap.id :: boxPath
    topics =
      VM.topicsToRender topicMap model |> List.map
        (\({id} as mapTopic) ->
          case Topic.fromId id model of
            Just topic -> viewTopic topic mapTopic newPath env
            _ -> U.logError "TopicMap.View.viewItems"
              ("problem with topic " ++ fromInt (toTopicId id)) (text "")
        )
    assocs =
      Box.assocIds topicMap.id model |> List.foldr
        (\id svgAcc ->
          case Assoc.fromId id model of
            Just assoc ->
              let
                clickHandler = assocClickHandler id newPath
              in
              svgAcc ++ viewAssoc assoc newPath clickHandler model
            _ -> U.logError "TopicMap.View.viewItems"
              ("problem with assoc " ++ fromInt (toAssocId id)) svgAcc
        )
        []
  in
  (topics, assocs)


assocClickHandler : AssocId -> BoxPath -> Attrs Msg
assocClickHandler assocId boxPath =
  [ Events.onClickStop (Model.TopicMap <| TopicMapDef.AssocClicked assocId boxPath) ]


viewLimboAssoc : BoxId -> Model -> List (Html Msg)
viewLimboAssoc boxId model =
  case VM.limboState model of
    Just (topicId, Just assocId, limboBoxId) ->
      if boxId == limboBoxId then
        if Box.hasItem (A assocId) boxId model then
          let
            _ = U.info "TopicMap.View.viewLimboAssoc" (assocId, "is in map", boxId)
          in
          [] -- rendered already (viewItems())
        else
          let
            _ = U.info "TopicMap.View.viewLimboAssoc" (assocId, "not in map", boxId)
          in
          case Assoc.fromId assocId model of
            Just assoc ->
              -- only if connected topic has geometry in this map already we can call
              -- viewAssoc()
              if TM.hasMapTopic topicId boxId model then
                viewAssoc assoc [boxId] [] model -- simple box path is sufficient for geometry,
                                                 -- limbo assoc is never selected
              else
                -- otherwise we call low-level lineRenderer() with topic default position
                Assoc.otherTopicId assocId topicId model
                  |> Maybe.andThen
                    (\sourceTopicId -> TM.topicPos sourceTopicId boxId model)
                  |> Maybe.andThen
                    (\pos -> Just
                      ( (lineRenderer model)
                          pos (TM.initTopicPos boxId model) (Just assoc)
                          [boxId] [] model -- simple box path is sufficient for geometry,
                                           -- limbo assoc is never selected
                      )
                    )
                  |> Maybe.withDefault []
            Nothing -> []
      else
        []
    _ -> []


-- Topic Rendering

viewTopic : Topic -> MapTopic -> BoxPath -> Env2 -> Html Msg
viewTopic topic mapTopic boxPath ({model, ext}) =
  let
    render =
      case (Topic.isBox topic.id model, mapTopic.expansion) of
        (False, Collapsed) -> labelTopic topic mapTopic boxPath
        (False, Expanded) -> detailTopic topic mapTopic boxPath
        (True, Collapsed) -> blackBoxTopic topic mapTopic boxPath
        (True, Expanded) -> whiteBoxTopic topic mapTopic boxPath ext
    (style, children) = render model
  in
  div
    ( topicAttr topic.id boxPath
      ++ Events.draggable topic.id boxPath boxPath
      ++ topicStyle topic.id boxPath model
      ++ style
    )
    ( children
      ++ Tool.viewTopicTools topic.id boxPath model
    )


topicAttr : TopicId -> BoxPath -> Attrs Msg
topicAttr topicId boxPath =
  [ id <| Box.elemId "topic" topicId boxPath ]


topicStyle : TopicId -> BoxPath -> Model -> Attrs Msg
topicStyle id boxPath model =
  let
    boxId = Box.firstId boxPath
    isLimbo = VM.isLimboTopic id boxId model
    isDragging = Mouse.isTopicDragging id boxPath model
    isSelected = Sel.isSelected (T id) boxPath model
  in
  [ style "position" "absolute"
  , style "filter" <| if isLimbo then C.topicLimboFilter else "none"
  , style "z-index" <| if isDragging then "1" else if isSelected then "3" else "2"
  , style "touch-action" "none"
  ]


labelTopic : Topic -> MapTopic -> BoxPath -> Model -> TopicRendering
labelTopic topic mapTopic boxPath model =
  ( topicPosStyle mapTopic
      ++ topicFlexboxStyle mapTopic boxPath model
      ++ VB.selectionStyle topic.id boxPath model
  , viewLabelTopic topic mapTopic boxPath model
  )


viewLabelTopic : Topic -> MapTopic -> BoxPath -> Model -> List (Html Msg)
viewLabelTopic topic mapTopic boxPath model =
  let
    textElem =
      if Text.isEdit topic.id boxPath model then
        Text.viewInput topic boxPath inputStyle
      else
        div
          labelTopicStyle
          [ text <| Topic.label topic ]
  in
  [ div
    (iconBoxStyle mapTopic model)
    [ Icon.viewTopicIcon topic.id C.topicIconSize topicIconStyle model ]
  , textElem
  ]


inputStyle : Attrs Msg
inputStyle =
  [ style "font-family" C.mainFont -- Default for <input> is "-apple-system" (on Mac)
  , style "font-size" <| fromInt C.contentFontSize ++ "px"
  , style "font-weight" C.topicLabelWeight
  , style "width" "100%"
  , style "position" "relative"
  , style "left" "-4px"
  ]


labelTopicStyle : Attrs Msg
labelTopicStyle =
  [ style "font-size" <| fromInt C.contentFontSize ++ "px"
  , style "font-weight" C.topicLabelWeight
  , style "overflow" "hidden"
  , style "text-overflow" "ellipsis"
  , style "white-space" "nowrap"
  ]


detailTopic : Topic -> MapTopic -> BoxPath -> Model -> TopicRendering
detailTopic topic mapTopic boxPath model =
  let
    textElem =
      case Text.isEdit topic.id boxPath model of
        True ->
          Text.viewTextarea topic boxPath
            ( detailTextStyle topic.id boxPath model
              ++ textEditorStyle topic.id model
            )
        False ->
          div
            ( detailTextStyle topic.id boxPath model
              ++ textViewStyle
            )
            ( Text.markdown topic.text model )
  in
  ( detailTopicStyle mapTopic
  , [ div
      ( iconBoxStyle mapTopic model
        ++ detailTopicIconBoxStyle
        ++ VB.selectionStyle topic.id boxPath model
      )
      [ Icon.viewTopicIcon topic.id C.topicIconSize topicIconStyle model ]
    , textElem
    ]
  )


detailTopicStyle : MapTopic -> Attrs Msg
detailTopicStyle {pos} =
  [ style "display" "flex"
  , style "left" <| fromInt (pos.x - C.topicW2) ++ "px"
  , style "top" <| fromInt (pos.y - C.topicH2) ++ "px"
  ]


detailTextStyle : TopicId -> BoxPath -> Model -> Attrs Msg
detailTextStyle topicId boxPath model =
  let
    r = fromInt C.topicRadius ++ "px"
  in
  [ style "font-size" <| fromInt C.contentFontSize ++ "px"
  , style "width" <| fromInt C.topicDetailMaxWidth ++ "px"
  , style "line-height" <| fromFloat C.topicLineHeight
  , style "padding" <| fromInt C.topicDetailPadding ++ "px"
  , style "border-radius" <| "0 " ++ r ++ " " ++ r ++ " " ++ r
  ]
  ++ VB.topicBorderStyle topicId boxPath model
  ++ VB.selectionStyle topicId boxPath model


textViewStyle : Attrs Msg
textViewStyle =
  [ style "min-width" <| fromInt (C.topicSize.w - C.topicSize.h) ++ "px"
  , style "max-width" "max-content"
  ]


textEditorStyle : TopicId -> Model -> Attrs Msg
textEditorStyle topicId model =
  let
    height = case Topic.size topicId .editor model of
      Just size -> size.h
      Nothing -> 0
  in
  [ style "position" "relative"
  , style "top" <| fromInt -C.topicBorderWidth ++ "px"
  , style "height" <| fromInt height ++ "px"
  , style "font-family" C.editorFont
  , style "border-color" "black" -- <textarea> default is some lightgray
  , style "resize" "none"
  ]


iconBoxStyle : MapTopic -> Model -> Attrs Msg
iconBoxStyle mapTopic model =
  let
    r1 = fromInt C.topicRadius ++ "px"
    r4 =
      case (Topic.isBox mapTopic.id model, mapTopic.expansion) of
        (True, Expanded) -> "0"
        _ -> r1
  in
  [ style "flex" "none"
  , style "width" <| fromInt C.topicSize.h ++ "px"
  , style "height" <| fromInt C.topicSize.h ++ "px"
  , style "border-radius" <| r1 ++ " 0 0 " ++ r4
  , style "background-color" "black"
  ]


detailTopicIconBoxStyle : Attrs Msg
detailTopicIconBoxStyle =
  -- icon box correction as detail topic has no border, in contrast to label topic
  [ style "padding-left" <| fromInt C.topicBorderWidth ++ "px"
  , style "width" <| fromInt (C.topicSize.h - C.topicBorderWidth) ++ "px"
  ]


topicIconStyle : Attrs Msg
topicIconStyle =
  [ style "position" "relative"
  , style "top" <| fromInt ((C.topicSize.h - C.topicIconSize) // 2) ++ "px"
  , style "left" <| fromInt ((C.topicSize.h - C.topicIconSize) // 2) ++ "px"
  , style "color" "white"
  ]


blackBoxTopic : Topic -> MapTopic -> BoxPath -> Model -> TopicRendering
blackBoxTopic topic mapTopic boxPath model =
  ( topicPosStyle mapTopic
  , [ div
      (topicFlexboxStyle mapTopic boxPath model)
      (viewLabelTopic topic mapTopic boxPath model
        ++ viewItemCount (BoxId topic.id) model -- topic has proven to be a box by caller
      )
    , div
      (ghostTopicStyle topic boxPath model)
      []
    ]
  )


topicPosStyle : MapTopic -> Attrs Msg
topicPosStyle { pos } =
  [ style "left" <| fromInt (pos.x - C.topicW2) ++ "px"
  , style "top" <| fromInt (pos.y - C.topicH2) ++ "px"
  ]


topicFlexboxStyle : MapTopic -> BoxPath -> Model -> Attrs Msg
topicFlexboxStyle mapTopic boxPath model =
  let
    r12 = fromInt C.topicRadius ++ "px"
    r34 =
      case (Topic.isBox mapTopic.id model, mapTopic.expansion) of
        (True, Expanded) -> "0"
        _ -> r12
  in
  [ style "display" "flex"
  , style "align-items" "center"
  , style "gap" "8px"
  , style "width" <| fromInt C.topicSize.w ++ "px"
  , style "height" <| fromInt C.topicSize.h ++ "px"
  , style "border-radius" <| r12 ++ " " ++ r12 ++ " " ++ r34 ++ " " ++ r34
  ]
  ++ VB.topicBorderStyle mapTopic.id boxPath model


ghostTopicStyle : Topic -> BoxPath -> Model -> Attrs Msg
ghostTopicStyle topic boxPath model =
  [ style "position" "absolute"
  , style "left" <| fromInt C.blackBoxOffset ++ "px"
  , style "top" <| fromInt C.blackBoxOffset ++ "px"
  , style "width" <| fromInt C.topicSize.w ++ "px"
  , style "height" <| fromInt C.topicSize.h ++ "px"
  , style "border-radius" <| fromInt C.topicRadius ++ "px"
  , style "z-index" "-1" -- behind topic
  ]
  ++ VB.topicBorderStyle topic.id boxPath model
  ++ VB.selectionStyle topic.id boxPath model


whiteBoxTopic : Topic -> MapTopic -> BoxPath -> ExtManager -> Model -> TopicRendering
whiteBoxTopic topic mapTopic boxPath ext model =
  let
    (style, children) = labelTopic topic mapTopic boxPath model
  in
  ( style
  , children
    ++ viewItemCount (BoxId topic.id) model -- topic has proven to be a box by caller
    ++ [ ext.view (BoxId topic.id) boxPath model ]
  )


viewItemCount : BoxId -> Model -> List (Html Msg)
viewItemCount boxId model =
  [ div
      itemCountStyle
      [ text (fromInt (Box.topicCount boxId model)) ]
  ]


itemCountStyle : Attrs Msg
itemCountStyle =
  [ style "font-size" <| fromInt C.contentFontSize ++ "px"
  , style "position" "absolute"
  , style "left" "calc(100% + 12px)"
  ]


-- Association Rendering

viewAssoc : Assoc -> BoxPath -> Attrs Msg -> Model -> List (Svg Msg)
viewAssoc assoc boxPath clickHandler model =
  let
    boxId = Box.firstId boxPath
    geom = TM.assocGeometry assoc boxId model
  in
  case geom of
    Just (pos1, pos2) -> (lineRenderer model) pos1 pos2 (Just assoc) boxPath clickHandler model
    Nothing -> []


viewAssocDraft : BoxId -> Model -> List (Svg Msg)
viewAssocDraft boxId model =
  case (model.mouse.dragSource, model.topicMap.dragState) of
    (Just {topicId, boxPath, lastPointerPos}, Just (Drag DraftAssoc)) ->
      let
        sourceBoxId = Box.firstId boxPath
      in
      if sourceBoxId == boxId then
        case TM.topicPos topicId sourceBoxId model of
          Just pos1 ->
            let
              pos2 = VM.toLocalPos lastPointerPos boxPath model
            in
            (lineRenderer model) pos1 pos2 Nothing [boxId] [] model
            -- simple box path is sufficient for geometry, draft assoc is never selected
          Nothing -> []
      else
        []
    _ -> []


lineRenderer : Model -> LineRenderer
lineRenderer model =
  case model.tool.lineStyle of
    Cornered -> corneredLine
    Straight -> straightLine


straightLine : LineRenderer
straightLine pos1 pos2 maybeAssoc boxPath clickHandler model =
  viewLineWithHitArea
    Svg.line
      [ x1 <| fromInt pos1.x
      , y1 <| fromInt pos1.y
      , x2 <| fromInt pos2.x
      , y2 <| fromInt pos2.y
      ]
      clickHandler maybeAssoc boxPath model


corneredLine : LineRenderer
corneredLine pos1 pos2 maybeAssoc boxPath clickHandler model =
  -- straight vertical
  if abs (pos2.x - pos1.x) < 2 * C.assocRadius then
    let
      xm = (pos1.x + pos2.x) // 2
    in
    viewLineWithHitArea
      Svg.path
        [ d ("M " ++ fromInt xm ++ " " ++ fromInt pos1.y ++ " V " ++ fromInt pos2.y) ]
        clickHandler maybeAssoc boxPath model
  -- straight horizontal
  else if abs (pos2.y - pos1.y) < 2 * C.assocRadius then
    let
      ym = (pos1.y + pos2.y) // 2
    in
    viewLineWithHitArea
      Svg.path
        [ d ("M " ++ fromInt pos1.x ++ " " ++ fromInt ym ++ " H " ++ fromInt pos2.x) ]
        clickHandler maybeAssoc boxPath model
  -- 5 segment cornered line
  else
    let
      sx = if pos2.x > pos1.x then 1 else -1 -- sign x
      sy = if pos2.y > pos1.y then -1 else 1 -- sign y
      ym = (pos1.y + pos2.y) // 2 -- y mean
      x1 = fromInt (pos1.x + sx * C.assocRadius)
      x2 = fromInt (pos2.x - sx * C.assocRadius)
      y1 = fromInt (ym + sy * C.assocRadius)
      y2 = fromInt (ym - sy * C.assocRadius)
      sweep1 =
        if sy == 1 then
          if sx == 1 then 1 else 0
        else
          if sx == 1 then 0 else 1
      sweep2 = 1 - sweep1
      sw1 = fromInt sweep1
      sw2 = fromInt sweep2
      r = fromInt C.assocRadius
    in
    viewLineWithHitArea
      Svg.path
        [ d
            ( "M " ++ fromInt pos1.x ++ " " ++ fromInt pos1.y ++
              " V " ++ y1 ++
              " A " ++ r ++ " " ++ r ++ " 0 0 " ++ sw1 ++ " " ++ x1 ++ " " ++ fromInt ym ++
              " H " ++ x2 ++
              " A " ++ r ++ " " ++ r ++ " 0 0 " ++ sw2 ++ " " ++ fromInt pos2.x ++ " " ++ y2 ++
              " V " ++ fromInt pos2.y
            )
        ]
        clickHandler maybeAssoc boxPath model


type alias LineWithHitArea =
  SvgElement -> Attrs Msg -> Attrs Msg -> Maybe Assoc -> BoxPath -> Model -> List (Svg Msg)


type alias SvgElement = Attrs Msg -> List (Svg Msg) -> Svg Msg


viewLineWithHitArea : LineWithHitArea
viewLineWithHitArea svgElement geometry clickHandler maybeAssoc boxPath model =
  let
    boxId = Box.firstId boxPath
  in
  [ svgElement -- visible line
      ( geometry
        ++ lineStyle maybeAssoc boxId model
        ++ lineSelectionStyle maybeAssoc boxPath model
      )
      []
  , svgElement -- hit area
      ( geometry
        ++ clickHandler
        ++ lineHitAreaStyle
      )
      []
  ]


lineStyle : Maybe Assoc -> BoxId -> Model -> Attrs Msg
lineStyle assoc boxId model =
  let
    color =
      case assoc of
        Just {id} ->
          case VM.isLimboAssoc id boxId model of
            True -> C.assocLimboColor
            False -> C.assocColor
        Nothing -> C.assocLimboColor
  in
  [ stroke color
  , strokeWidth <| fromFloat C.assocWidth ++ "px"
  , strokeDasharray <| lineDasharray assoc
  , fill "none"
  ]


lineHitAreaStyle : Attrs Msg
lineHitAreaStyle =
  [ stroke "transparent"
  , strokeWidth <| fromInt C.assocHitArea ++ "px"
  , fill "none"
  ]


lineSelectionStyle : Maybe Assoc -> BoxPath -> Model -> Attrs Msg
lineSelectionStyle maybeAssoc boxPath model =
  case maybeAssoc of
    Just {id} ->
      case Sel.isSelected (A id) boxPath model of
        True -> [ filter "url(#shadow)" ]
        False -> []
    Nothing -> []


lineDasharray : Maybe Assoc -> String
lineDasharray assoc =
  case assoc of
    Just {assocType} ->
      case assocType of
        Association -> "5 0" -- solid
        Hierarchy -> "5" -- dotted
    Nothing -> "5 0" -- solid


-- Toolbar

toolbarPos : BoxPath -> Model -> ToolbarPos
toolbarPos boxPath model =
  let
    boxId = Box.firstId boxPath
  in
  case TM.byId boxId model of
    Just {rect} ->
      ToolbarPos
        (\topic ->
          case TM.topicPos topic.id boxId model of
            Just topicPos ->
              Point
                (topicPos.x - rect.x1 - C.topicW2)
                (topicPos.y - rect.y1 - C.topicH2 - 29) -- TODO: 29 ≈ toolbar height
            Nothing -> Point 0 0
        )
        (\assoc ->
          case TM.assocGeometry assoc boxId model of
            Just (p1, p2) ->
              Point
                ((p1.x + p2.x) // 2 - rect.x1 - 32) -- TODO: 32 ≈ toolbar width / 2
                ((p1.y + p2.y) // 2 - rect.y1 - 13) -- TODO: 13 ≈ toolbar height / 2
            Nothing -> Point 0 0
        )
    Nothing ->
      (ToolbarPos (\_ -> Point 0 0) (\_ -> Point 0 0))
