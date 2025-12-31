module Map exposing (view)

import Box
import Config as C
import Feature.IconAPI as IconAPI
import Feature.Mouse exposing (DragState(..), DragMode(..))
import Feature.MouseAPI as MouseAPI
import Feature.SelAPI as SelAPI
import Feature.TextAPI as TextAPI
import Feature.ToolAPI as ToolAPI
import Item
import Map.Model as MM
import Model exposing (Model, Msg)
import ModelParts exposing (..)
import Utils as U

import Dict
import Html exposing (Html, div, text)
import Html.Attributes exposing (id, style)
import String exposing (fromInt, fromFloat)
import Svg exposing (Svg, svg, g, line, path)
import Svg.Attributes exposing (width, height, x1, y1, x2, y2, d, stroke, fill, transform,
  strokeWidth, strokeDasharray)



-- CONFIG


lineFunc = taxiLine -- or directLine



-- MODEL


type alias BoxInfo =
  ( ( List (Html Msg), List (Svg Msg) )
  , Rectangle
  , ( { w: String, h: String }
    , Attributes Msg
    )
  )

type alias TopicRendering = (Attributes Msg, List (Html Msg))



-- VIEW


-- For a fullscreen box boxPath is empty
view : BoxId -> BoxPath -> Model -> Html Msg
view boxId boxPath model =
  let
    ((topics, assocs), boxRect, (svgSize, boxStyle)) = boxInfo boxId boxPath model
  in
  div
    boxStyle
    [ div
        ( topicLayerStyle boxRect )
        topics
    , svg
        ( [ width svgSize.w, height svgSize.h ]
          ++ svgStyle
        )
        [ g
          ( gAttr boxId boxRect model )
          ( assocs
            ++ viewLimboAssoc boxId model
            ++ viewAssocDraft boxId model
          )
        ]
    ]


topicLayerStyle : Rectangle -> Attributes Msg
topicLayerStyle boxRect =
  [ style "position" "absolute"
  , style "left" <| fromInt -boxRect.x1 ++ "px"
  , style "top" <| fromInt -boxRect.y1 ++ "px"
  ]


svgStyle : Attributes Msg
svgStyle =
  [ style "position" "absolute" -- occupy entire window height (instead 150px default height)
  , style "top" "0"
  , style "left" "0"
  ]


gAttr : BoxId -> Rectangle -> Model -> Attributes Msg
gAttr boxId boxRect model =
    [ transform
      <| "translate(" ++ fromInt -boxRect.x1 ++ " " ++ fromInt -boxRect.y1 ++ ")"
    ]


-- For a fullscreen box boxPath is empty
boxInfo : BoxId -> BoxPath -> Model -> BoxInfo
boxInfo boxId boxPath model =
  case Box.byIdOrLog boxId model of
    Just box ->
      ( viewItems box boxPath model
      , box.rect
      , ( { w = (box.rect.x2 - box.rect.x1) |> fromInt
          , h = (box.rect.y2 - box.rect.y1) |> fromInt
          }
        , if model.boxId == boxId then
            []
          else
            nestedBoxStyle boxId box.rect boxPath model
        )
      )
    Nothing ->
      U.fail "boxInfo" {boxId = boxId, boxPath = boxPath}
        ( ([], []), Rectangle 0 0 0 0, ( {w = "0", h = "0"}, [] ))


nestedBoxStyle : Id -> Rectangle -> BoxPath -> Model -> Attributes Msg
nestedBoxStyle topicId rect boxPath model =
  let
    width = rect.x2 - rect.x1
    height = rect.y2 - rect.y1
    r = fromInt C.whiteBoxRadius ++ "px"
  in
  [ style "position" "absolute"
  , style "left" <| fromInt -C.topicBorderWidth ++ "px"
  , style "top" <| fromInt (C.topicSize.h - 2 * C.topicBorderWidth) ++ "px"
  , style "width" <| fromInt width ++ "px"
  , style "height" <| fromInt height ++ "px"
  , style "border-radius" <| "0 " ++ r ++ " " ++ r ++ " " ++ r
  ]
  ++ topicBorderStyle topicId boxPath model
  ++ selectionStyle topicId boxPath model


-- For a fullscreen box boxPath is empty
viewItems : Box -> BoxPath -> Model -> (List (Html Msg), List (Svg Msg))
viewItems box boxPath model =
  let
    newPath = box.id :: boxPath
    topics =
      MM.topicsToRender box model |> List.map
        (\{id, props} ->
          case (Item.topicById id model, props) of
            (Just topic, TopicP tProps) -> viewTopic topic tProps newPath model
            _ -> U.logError "viewItems" ("problem with topic " ++ fromInt id) (text "")
        )
    assocs =
      MM.assocsToRender box model |> List.map
        (\{id} ->
          case Item.assocById id model of
            Just assoc -> viewAssoc assoc box.id model
            _ -> U.logError "viewItems" ("problem with assoc " ++ fromInt id) (text "")
        )
  in
  (topics, assocs)


viewLimboAssoc : BoxId -> Model -> List (Html Msg)
viewLimboAssoc boxId model =
  case MM.limboState model of
    Just (topicId, Just assocId, limboBoxId) ->
      if boxId == limboBoxId then
        if Box.hasItem boxId assocId model then
          let
            _ = U.info "viewLimboAssoc" (assocId, "is in box", boxId)
          in
          [] -- rendered already (viewItems())
        else
          let
            _ = U.info "viewLimboAssoc" (assocId, "not in box", boxId)
          in
          case Item.assocById assocId model of
            Just assoc ->
              if Box.hasItem boxId topicId model then
                -- only if related topic is in box we can call high-level viewAssoc()
                [ viewAssoc assoc boxId model ]
              else
                -- otherwise we call low-level lineFunc() with topic default position
                let
                  sourceTopicId = Item.otherPlayerId assocId topicId model
                in
                case Box.topicPos sourceTopicId boxId model of
                  Just pos ->
                    [ lineFunc pos (Box.initTopicPos boxId model) (Just assoc) boxId model ]
                  Nothing -> []
            Nothing -> []
      else
        []
    _ -> []


viewTopic : TopicInfo -> TopicProps -> BoxPath -> Model -> Html Msg
viewTopic topic props boxPath model =
  let
    boxId = Box.firstId boxPath
    render =
      case props.displayMode of
        TopicD LabelOnly -> labelTopic
        TopicD Detail -> detailTopic
        BoxD BlackBox -> blackBoxTopic
        BoxD WhiteBox -> whiteBoxTopic
        BoxD Unboxed -> unboxedTopic
    (style, children) = render topic props boxPath model
  in
  div
    ( topicAttr topic.id boxPath
      ++ MouseAPI.hoverHandler topic.id boxPath
      ++ MouseAPI.mouseDownHandler topic.id boxPath
      ++ topicStyle topic.id boxId model
      ++ style
    )
    ( children
      ++ ToolAPI.viewItemTools topic.id boxPath model
    )


topicAttr : Id -> BoxPath -> Attributes Msg
topicAttr topicId boxPath =
  [ id <| Box.elemId "topic" topicId boxPath ]


topicStyle : Id -> BoxId -> Model -> Attributes Msg
topicStyle id boxId model =
  let
    isLimbo = MM.isLimboTopic id boxId model
    isDragging = case model.mouse.dragState of
      Drag DragTopic id_ _ _ _ _ -> id_ == id
      _ -> False
    isSelected = SelAPI.isSelected id boxId model
  in
  [ style "position" "absolute"
  , style "filter" <| if isLimbo then C.topicLimboFilter else "none"
  , style "z-index" <| if isDragging then "1" else if isSelected then "3" else "2"
  ]


labelTopic : TopicInfo -> TopicProps -> BoxPath -> Model -> TopicRendering
labelTopic topic props boxPath model =
  ( topicPosStyle props
      ++ topicFlexboxStyle topic props boxPath model
      ++ selectionStyle topic.id boxPath model
  , viewLabelTopic topic props boxPath model
  )


inputStyle : Attributes Msg
inputStyle =
  [ style "font-family" C.mainFont -- Default for <input> is "-apple-system" (on Mac)
  , style "font-size" <| fromInt C.contentFontSize ++ "px"
  , style "font-weight" C.topicLabelWeight
  , style "width" "100%"
  , style "position" "relative"
  , style "left" "-4px"
  ]


topicLabelStyle : Attributes Msg
topicLabelStyle =
  [ style "font-size" <| fromInt C.contentFontSize ++ "px"
  , style "font-weight" C.topicLabelWeight
  , style "overflow" "hidden"
  , style "text-overflow" "ellipsis"
  , style "white-space" "nowrap"
  ]


viewLabelTopic : TopicInfo -> TopicProps -> BoxPath -> Model -> List (Html Msg)
viewLabelTopic topic props boxPath model =
  let
    textElem =
      case TextAPI.isEdit topic.id boxPath model of
        True ->
          TextAPI.viewInput topic boxPath inputStyle
        False ->
          div
            topicLabelStyle
            [ text <| Item.topicLabel topic ]
  in
  [ div
    (iconBoxStyle props)
    [ IconAPI.viewTopicIcon topic.id C.topicIconSize topicIconStyle model ]
  , textElem
  ]


detailTopic : TopicInfo -> TopicProps -> BoxPath -> Model -> TopicRendering
detailTopic topic props boxPath model =
  let
    textElem =
      case TextAPI.isEdit topic.id boxPath model of
        True ->
          TextAPI.viewTextarea topic boxPath
            ( detailTextStyle topic.id boxPath model
              ++ textEditorStyle topic.id model
            )
        False ->
          div
            ( detailTextStyle topic.id boxPath model
              ++ textViewStyle
            )
            ( TextAPI.markdown topic.text model )
  in
  ( detailTopicStyle props
  , [ div
      ( iconBoxStyle props
        ++ detailTopicIconBoxStyle
        ++ selectionStyle topic.id boxPath model
      )
      [ IconAPI.viewTopicIcon topic.id C.topicIconSize topicIconStyle model ]
    , textElem
    ]
  )


detailTopicStyle : TopicProps -> Attributes Msg
detailTopicStyle {pos} =
  [ style "display" "flex"
  , style "left" <| fromInt (pos.x - C.topicW2) ++ "px"
  , style "top" <| fromInt (pos.y - C.topicH2) ++ "px"
  ]


detailTextStyle : Id -> BoxPath -> Model -> Attributes Msg
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
  ++ topicBorderStyle topicId boxPath model
  ++ selectionStyle topicId boxPath model


textViewStyle : Attributes Msg
textViewStyle =
  [ style "min-width" <| fromInt (C.topicSize.w - C.topicSize.h) ++ "px"
  , style "max-width" "max-content"
  ]


textEditorStyle : Id -> Model -> Attributes Msg
textEditorStyle topicId model =
  let
    height = case Item.topicSize topicId .editor model of
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


topicIconStyle : Attributes Msg
topicIconStyle =
  [ style "position" "relative"
  , style "top" <| fromInt ((C.topicSize.h - C.topicIconSize) // 2) ++ "px"
  , style "left" <| fromInt ((C.topicSize.h - C.topicIconSize) // 2) ++ "px"
  , style "color" "white"
  ]


blackBoxTopic : TopicInfo -> TopicProps -> BoxPath -> Model -> TopicRendering
blackBoxTopic topic props boxPath model =
  ( topicPosStyle props
  , [ div
      (topicFlexboxStyle topic props boxPath model)
      (viewLabelTopic topic props boxPath model
        ++ viewItemCount topic.id props model
      )
    , div
      (ghostTopicStyle topic boxPath model)
      []
    ]
  )


topicFlexboxStyle : TopicInfo -> TopicProps -> BoxPath -> Model -> Attributes Msg
topicFlexboxStyle topic props boxPath model =
  let
    r12 = fromInt C.topicRadius ++ "px"
    r34 = case props.displayMode of
      BoxD WhiteBox -> "0"
      _ -> r12
  in
  [ style "display" "flex"
  , style "align-items" "center"
  , style "gap" "8px"
  , style "width" <| fromInt C.topicSize.w ++ "px"
  , style "height" <| fromInt C.topicSize.h ++ "px"
  , style "border-radius" <| r12 ++ " " ++ r12 ++ " " ++ r34 ++ " " ++ r34
  ]
  ++ topicBorderStyle topic.id boxPath model


ghostTopicStyle : TopicInfo -> BoxPath -> Model -> Attributes Msg
ghostTopicStyle topic boxPath model =
  [ style "position" "absolute"
  , style "left" <| fromInt C.blackBoxOffset ++ "px"
  , style "top" <| fromInt C.blackBoxOffset ++ "px"
  , style "width" <| fromInt C.topicSize.w ++ "px"
  , style "height" <| fromInt C.topicSize.h ++ "px"
  , style "border-radius" <| fromInt C.topicRadius ++ "px"
  , style "z-index" "-1" -- behind topic
  ]
  ++ topicBorderStyle topic.id boxPath model
  ++ selectionStyle topic.id boxPath model


whiteBoxTopic : TopicInfo -> TopicProps -> BoxPath -> Model -> TopicRendering
whiteBoxTopic topic props boxPath model =
  let
    (style, children) = labelTopic topic props boxPath model
  in
  ( style
  , children
    ++ viewItemCount topic.id props model
    ++ [ view topic.id boxPath model ]
  )


unboxedTopic : TopicInfo -> TopicProps -> BoxPath -> Model -> TopicRendering
unboxedTopic topic props boxPath model =
  let
    (style, children) = labelTopic topic props boxPath model
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
        TopicD _ -> 0
        BoxD _ ->
          case Box.byIdOrLog topicId model of
            Just box -> box.items |> Dict.values |> List.filter Box.isVisible |> List.length
            Nothing -> 0
  in
  [ div
      itemCountStyle
      [ text <| fromInt itemCount ]
  ]


itemCountStyle : Attributes Msg
itemCountStyle =
  [ style "font-size" <| fromInt C.contentFontSize ++ "px"
  , style "position" "absolute"
  , style "left" "calc(100% + 12px)"
  ]


viewAssoc : AssocInfo -> BoxId -> Model -> Svg Msg
viewAssoc assoc boxId model =
  let
    geom = assocGeometry assoc boxId model
  in
  case geom of
    Just (pos1, pos2) -> lineFunc pos1 pos2 (Just assoc) boxId model
    Nothing -> text "" -- TODO


assocGeometry : AssocInfo -> BoxId -> Model -> Maybe (Point, Point)
assocGeometry assoc boxId model =
  let
    pos1 = Box.topicPos assoc.player1 boxId model
    pos2 = Box.topicPos assoc.player2 boxId model
  in
  case Maybe.map2 (\p1 p2 -> (p1, p2)) pos1 pos2 of
    Just geometry -> Just geometry
    Nothing -> U.fail "assocGeometry" { assoc = assoc, boxId = boxId } Nothing


viewAssocDraft : BoxId -> Model -> List (Svg Msg)
viewAssocDraft boxId model =
  case model.mouse.dragState of
    Drag DraftAssoc _ boxPath origPos pos _ ->
      case (Box.firstId boxPath == boxId, Box.byIdOrLog model.boxId model) of
        (True, Just box) ->
          let
            pagePos = Point
              (pos.x + box.scroll.x)
              (pos.y + box.scroll.y - C.appHeaderHeight)
          in
          [ lineFunc origPos (relPos pagePos boxPath model) Nothing boxId model ]
        _ -> []
    _ -> []


{-| Transforms a viewport position to a box-relative position.
-}
relPos : Point -> BoxPath -> Model -> Point
relPos pos boxPath model =
  let
    posAbs = absPos boxPath (Point 0 0) model
  in
  Point
    (pos.x - posAbs.x)
    (pos.y - posAbs.y)


{-| Recursively calculates the absolute position of a box.
"posAcc" is the position accumulated so far.
-}
absPos : BoxPath -> Point -> Model -> Point
absPos boxPath posAcc model =
  case boxPath of
    [ boxId ] -> accumulateRect posAcc boxId model
    boxId :: parentBoxId :: boxIds -> accumulatePos posAcc boxId parentBoxId boxIds model
    [] -> U.logError "absPos" "boxPath is empty!" (Point 0 0)


accumulatePos : Point -> BoxId -> BoxId -> BoxPath -> Model -> Point
accumulatePos posAcc boxId parentBoxId boxIds model =
  let
    {x, y} = accumulateRect posAcc boxId model
  in
  case Box.topicPos boxId parentBoxId model of
    Just boxPos ->
      absPos -- recursion
        (parentBoxId :: boxIds)
        (Point
          (x + boxPos.x - C.topicW2)
          (y + boxPos.y + C.topicH2)
        )
        model
    Nothing -> Point 0 0 -- error is already logged


accumulateRect : Point -> BoxId -> Model -> Point
accumulateRect posAcc boxId model =
  case Box.byIdOrLog boxId model of
    Just box -> Point
      (posAcc.x - box.rect.x1)
      (posAcc.y - box.rect.y1)
    Nothing -> Point 0 0 -- error is already logged



-- STYLE


topicPosStyle : TopicProps -> Attributes Msg
topicPosStyle { pos } =
  [ style "left" <| fromInt (pos.x - C.topicW2) ++ "px"
  , style "top" <| fromInt (pos.y - C.topicH2) ++ "px"
  ]


iconBoxStyle : TopicProps -> Attributes Msg
iconBoxStyle props =
  let
    r1 = fromInt C.topicRadius ++ "px"
    r4 = case props.displayMode of
      BoxD WhiteBox -> "0"
      _ -> r1
  in
  [ style "flex" "none"
  , style "width" <| fromInt C.topicSize.h ++ "px"
  , style "height" <| fromInt C.topicSize.h ++ "px"
  , style "border-radius" <| r1 ++ " 0 0 " ++ r4
  , style "background-color" "black"
  ]


detailTopicIconBoxStyle : Attributes Msg
detailTopicIconBoxStyle =
  -- icon box correction as detail topic has no border, in contrast to label topic
  [ style "padding-left" <| fromInt C.topicBorderWidth ++ "px"
  , style "width" <| fromInt (C.topicSize.h - C.topicBorderWidth) ++ "px"
  ]


topicBorderStyle : Id -> BoxPath -> Model -> Attributes Msg
topicBorderStyle id boxPath model =
  let
    isTarget_ = isTarget id boxPath
    targeted = case model.mouse.dragState of
      -- can't move a topic to a box where it is already, can happen if mouse moves very quick
      -- can't create assoc when both topics are in different box
      Drag DragTopic _ (boxId_ :: _) _ _ target -> isTarget_ target && boxId_ /= id
      Drag DraftAssoc _ boxPath_ _ _ target -> isTarget_ target && boxPath_ == boxPath
      _ -> False
  in
  [ style "border-width" <| fromInt C.topicBorderWidth ++ "px"
  , style "border-style" <| if targeted then "dashed" else "solid"
  , style "box-sizing" "border-box"
  , style "background-color" "white"
  ]


selectionStyle : Id -> BoxPath -> Model -> Attributes Msg
selectionStyle topicId boxPath model =
  case SelAPI.isSelectedPath topicId boxPath model of
    True -> [ style "box-shadow" "gray 5px 5px 5px" ]
    False -> []


isTarget : Id -> BoxPath -> Maybe (Id, BoxPath) -> Bool
isTarget topicId boxPath target_ =
  case target_ of
    Just target -> target == (topicId, boxPath)
    Nothing -> False


-- One possible lineFunc
directLine : Point -> Point -> Maybe AssocInfo -> BoxId -> Model -> Svg Msg
directLine pos1 pos2 assoc boxId model =
  line
    ( [ x1 <| fromInt pos1.x
      , y1 <| fromInt pos1.y
      , x2 <| fromInt pos2.x
      , y2 <| fromInt pos2.y
      ] ++ lineStyle assoc boxId model
    )
    []


-- One possible lineFunc
taxiLine : Point -> Point -> Maybe AssocInfo -> BoxId -> Model -> Svg Msg
taxiLine pos1 pos2 assoc boxId model =
  if abs (pos2.x - pos1.x) < 2 * C.assocRadius then -- straight vertical
    let
      xm = (pos1.x + pos2.x) // 2
    in
    path
      ( [ d ("M " ++ fromInt xm ++ " " ++ fromInt pos1.y ++ " V " ++ fromInt pos2.y)
        ] ++ lineStyle assoc boxId model
      )
      []
  else if abs (pos2.y - pos1.y) < 2 * C.assocRadius then -- straight horizontal
    let
      ym = (pos1.y + pos2.y) // 2
    in
    path
      ( [ d ("M " ++ fromInt pos1.x ++ " " ++ fromInt ym ++ " H " ++ fromInt pos2.x)
        ] ++ lineStyle assoc boxId model
      )
      []
  else -- 5 segment taxi line
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
    path
      ( [ d
          ( "M " ++ fromInt pos1.x ++ " " ++ fromInt pos1.y ++
            " V " ++ y1 ++
            " A " ++ r ++ " " ++ r ++ " 0 0 " ++ sw1 ++ " " ++ x1 ++ " " ++ fromInt ym ++
            " H " ++ x2 ++
            " A " ++ r ++ " " ++ r ++ " 0 0 " ++ sw2 ++ " " ++ fromInt pos2.x ++ " " ++ y2 ++
            " V " ++ fromInt pos2.y
          )
        ] ++ lineStyle assoc boxId model
      )
      []


lineStyle : Maybe AssocInfo -> BoxId -> Model -> Attributes Msg
lineStyle assoc boxId model =
  let
    color =
      case assoc of
        Just {id} ->
          case MM.isLimboAssoc id boxId model of
            True -> C.assocLimboColor
            False -> C.assocColor
        Nothing -> C.assocLimboColor
  in
  [ stroke color
  , strokeWidth <| fromFloat C.assocWidth ++ "px"
  , strokeDasharray <| lineDasharray assoc
  , fill "none"
  ]


lineDasharray : Maybe AssocInfo -> String
lineDasharray assoc =
  case assoc of
    Just {assocType} ->
      case assocType of
        Crosslink -> "5 0" -- solid
        Hierarchy -> "5" -- dotted
    Nothing -> "5 0" -- solid
