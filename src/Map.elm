module Map exposing (view)

import Box
import Config as C
import Feature.IconAPI as IconAPI
import Feature.Mouse exposing (DragState(..), DragMode(..))
import Feature.MouseAPI as MouseAPI
import Feature.Search exposing (Menu(..))
import Feature.SelectionAPI as Sel
import Feature.TextEdit as T exposing (EditState(..))
import Feature.ToolAPI as ToolAPI
import Item
import Model exposing (Model, Msg(..))
import ModelParts exposing (..)
import Utils as U

import Dict
import Html exposing (Html, Attribute, div, text, input, textarea)
import Html.Attributes exposing (id, style, attribute, value)
import Html.Events exposing (onInput, onBlur)
import String exposing (fromInt, fromFloat)
import Svg exposing (Svg, svg, g, line, path)
import Svg.Attributes exposing (width, height, x1, y1, x2, y2, d, stroke, fill, transform,
  strokeWidth, strokeDasharray)



-- CONFIG


lineFunc = taxiLine -- directLine



-- MODEL


type alias BoxInfo =
  ( ( List (Html Msg), List (Svg Msg) )
  , Rectangle
  , ( { w: String, h: String }
    , List (Attribute Msg)
    )
  )

type alias TopicRendering = (List (Attribute Msg), List (Html Msg))



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
        ( topics
          ++ viewLimboTopic boxId model
        )
    , svg
        ( [ width svgSize.w, height svgSize.h ]
          ++ topicAttr boxId boxPath model
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


topicLayerStyle : Rectangle -> List (Attribute Msg)
topicLayerStyle boxRect =
  [ style "position" "absolute"
  , style "left" <| fromFloat -boxRect.x1 ++ "px"
  , style "top" <| fromFloat -boxRect.y1 ++ "px"
  ]


svgStyle : List (Attribute Msg)
svgStyle =
  [ style "position" "absolute" -- occupy entire window height (instead 150px default height)
  , style "top" "0"
  , style "left" "0"
  ]


gAttr : BoxId -> Rectangle -> Model -> List (Attribute Msg)
gAttr boxId boxRect model =
    [ transform
      <| "translate(" ++ fromFloat -boxRect.x1 ++ " " ++ fromFloat -boxRect.y1 ++ ")"
    ]


-- For a fullscreen box boxPath is empty
boxInfo : BoxId -> BoxPath -> Model -> BoxInfo
boxInfo boxId boxPath model =
  case Box.byIdOrLog boxId model.boxes of
    Just box ->
      ( viewItems box boxPath model
      , box.rect
      , ( { w = (box.rect.x2 - box.rect.x1) |> round |> fromInt
          , h = (box.rect.y2 - box.rect.y1) |> round |> fromInt
          }
        , if Box.isActive boxId model then
            []
          else
            nestedBoxStyle boxId box.rect (Box.firstId boxPath) model
        )
      )
    Nothing ->
      ( ([], []), Rectangle 0 0 0 0, ( {w = "0", h = "0"}, [] ))


nestedBoxStyle : Id -> Rectangle -> BoxId -> Model -> List (Attribute Msg)
nestedBoxStyle topicId rect boxId model =
  let
    width = rect.x2 - rect.x1
    height = rect.y2 - rect.y1
    r = fromInt C.whiteBoxRadius ++ "px"
  in
  [ style "position" "absolute"
  , style "left" <| fromFloat -C.topicBorderWidth ++ "px"
  , style "top" <| fromFloat (C.topicSize.h - 2 * C.topicBorderWidth) ++ "px"
  , style "width" <| fromFloat width ++ "px"
  , style "height" <| fromFloat height ++ "px"
  , style "border-radius" <| "0 " ++ r ++ " " ++ r ++ " " ++ r
  ]
  ++ topicBorderStyle topicId boxId model
  ++ selectionStyle topicId boxId model


-- For a fullscreen box boxPath is empty
viewItems : Box -> BoxPath -> Model -> (List (Html Msg), List (Svg Msg))
viewItems box boxPath model =
  let
    newPath = box.id :: boxPath
  in
  box.items |> Dict.values |> List.filter (shouldItemRender box.id model) |> List.foldr
    (\{id, props} (t, a) ->
      case Item.byId id model of
        Just {info} ->
          case (info, props) of
            (Topic topic, TopicV tProps) -> (viewTopic topic tProps newPath model :: t, a)
            (Assoc assoc, AssocV _) -> (t, viewAssoc assoc box.id model :: a)
            _ -> U.logError "viewItems" ("problem with item " ++ fromInt id) (t, a)
        Nothing -> U.logError "viewItems" ("problem with item " ++ fromInt id) (t, a)
    )
    ([], [])


shouldItemRender : BoxId -> Model -> BoxItem -> Bool
shouldItemRender boxId model item =
  Box.isVisible item || isLimboItem item boxId model


viewLimboTopic : BoxId -> Model -> List (Html Msg)
viewLimboTopic boxId model =
  case limboState model of
    Just (topicId, _, limboBoxId) ->
      if boxId == limboBoxId then
        if Box.hasItem boxId topicId model then
          let
            _ = U.info "viewLimboTopic" (topicId, "is in box", boxId)
          in
          [] -- rendered already (viewItems())
        else
          let
            _ = U.info "viewLimboTopic" (topicId, "not in box", boxId)
            props = Box.initTopicProps topicId boxId model
            boxPath = [boxId] -- Needed by limbo style calculation; single ID is sufficient;
          in
          case Item.topicById topicId model of
            Just topic -> [ viewTopic topic props boxPath model ]
            Nothing -> []
      else
        []
    Nothing -> []


viewLimboAssoc : BoxId -> Model -> List (Html Msg)
viewLimboAssoc boxId model =
  case limboState model of
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
                  Just pos -> [ lineFunc pos (Box.initPos boxId) (Just assoc) boxId model ]
                  Nothing -> []
            Nothing -> []
      else
        []
    _ -> []


isLimboItem : BoxItem -> BoxId -> Model -> Bool
isLimboItem item boxId model =
  let
    isLimbo =
      case item.props of
        TopicV _ -> isLimboTopic
        AssocV _ -> isLimboAssoc
  in
  isLimbo item.id boxId model


isLimboTopic : Id -> BoxId -> Model -> Bool
isLimboTopic topicId boxId model =
  case limboState model of
    Just (topicId_, _, boxId_) -> topicId == topicId_ && boxId == boxId_
    Nothing -> False


isLimboAssoc : Id -> BoxId -> Model -> Bool
isLimboAssoc assocId boxId model =
  case limboState model of
    Just (_, Just assocId_, boxId_) -> assocId == assocId_ && boxId == boxId_
    _ -> False


limboState : Model -> Maybe (Id, Maybe Id, BoxId) -- (topic ID, assoc ID, box ID)
limboState model =
  case model.search.menu of
    Topics _ (Just topicId) -> Just (topicId, Nothing, Box.active model)
    RelTopics _ (Just (topicId, assocId)) ->
      case Sel.singleBoxId model of
        Just boxId -> Just (topicId, Just assocId, boxId)
        Nothing -> Nothing
    _ -> Nothing


viewTopic : TopicInfo -> TopicProps -> BoxPath -> Model -> Html Msg
viewTopic topic props boxPath model =
  let
    boxId = Box.firstId boxPath
    topicFunc =
      case effectiveDisplayMode topic.id boxId props.displayMode model of
        TopicD LabelOnly -> labelTopic
        TopicD Detail -> detailTopic
        BoxD BlackBox -> blackBoxTopic
        BoxD WhiteBox -> whiteBoxTopic
        BoxD Unboxed -> unboxedTopic
    (style, children) = topicFunc topic props boxPath model
  in
  div
    ( topicAttr topic.id boxPath model
      ++ MouseAPI.hoverHandler
      ++ topicStyle topic.id boxId model
      ++ style
    )
    ( children
      ++ ToolAPI.viewTools topic.id boxId model
    )


topicAttr : Id -> BoxPath -> Model -> List (Attribute Msg)
topicAttr topicId boxPath model =
  if Box.isActive topicId model then
    [] -- TODO: the fullscreen box would require dedicated event handling, e.g. panning?
  else
    [ attribute "class" "dmx-topic"
    , attribute "data-id" (fromInt topicId)
    , attribute "data-path" (Box.fromPath boxPath)
    ]


topicStyle : Id -> BoxId -> Model -> List (Attribute Msg)
topicStyle id boxId model =
  let
    isDragging = case model.mouse.dragState of
      Drag DragTopic _ id_ _ _ _ _ -> id_ == id
      _ -> False
  in
  [ style "position" "absolute"
  , style "opacity" <| if isLimboTopic id boxId model then ".5" else "1"
  , style "z-index" <| if isDragging then "1" else "2"
  ]


effectiveDisplayMode : Id -> BoxId -> DisplayMode -> Model -> DisplayMode
effectiveDisplayMode topicId boxId displayMode model =
  if isLimboTopic topicId boxId model then
    case displayMode of
      TopicD _ -> TopicD Detail
      BoxD _ -> BoxD WhiteBox
  else
    displayMode


labelTopic : TopicInfo -> TopicProps -> BoxPath -> Model -> TopicRendering
labelTopic topic props boxPath model =
  let
    boxId = Box.firstId boxPath
  in
  ( topicPosStyle props
      ++ topicFlexboxStyle topic props boxId model
      ++ selectionStyle topic.id boxId model
  , labelTopicHtml topic props boxId model
  )


labelTopicHtml : TopicInfo -> TopicProps -> BoxId -> Model -> List (Html Msg)
labelTopicHtml topic props boxId model =
  let
    isEdit = model.edit.state == ItemEdit topic.id boxId
    textElem =
      if isEdit then
        input
          ( [ id <| "dmx-input-" ++ fromInt topic.id ++ "-" ++ fromInt boxId
            , value topic.text
            , onInput (Edit << T.OnTextInput)
            , onBlur (Edit T.EditEnd)
            , U.onEnterOrEsc (Edit T.EditEnd)
            , U.stopPropagationOnMousedown NoOp
            ]
            ++ topicInputStyle
          )
          []
      else
        div
          topicLabelStyle
          [ text <| Item.topicLabel topic ]
  in
  [ div
    (topicIconBoxStyle props)
    [ IconAPI.viewTopicIcon topic.id model ]
  , textElem
  ]


detailTopic : TopicInfo -> TopicProps -> BoxPath -> Model -> TopicRendering
detailTopic topic props boxPath model =
  let
    boxId = Box.firstId boxPath
    isEdit = model.edit.state == ItemEdit topic.id boxId
    textElem =
      if isEdit then
        textarea
          ( [ id <| "dmx-input-" ++ fromInt topic.id ++ "-" ++ fromInt boxId
            , onInput (Edit << T.OnTextareaInput)
            , onBlur (Edit T.EditEnd)
            , U.onEsc (Edit T.EditEnd)
            , U.stopPropagationOnMousedown NoOp
            ]
            ++ detailTextStyle topic.id boxId model
            ++ detailTextEditStyle topic.id boxId model
          )
          [ text topic.text ]
      else
        div
          ( detailTextStyle topic.id boxId model
            ++ detailTextViewStyle
          )
          ( U.multilineHtml topic.text )
  in
  ( detailTopicStyle props
  , [ div
      ( topicIconBoxStyle props
        ++ detailTopicIconBoxStyle
        ++ selectionStyle topic.id boxId model
      )
      [ IconAPI.viewTopicIcon topic.id model ]
    , textElem
    ]
  )


detailTopicStyle : TopicProps -> List (Attribute Msg)
detailTopicStyle {pos} =
  [ style "display" "flex"
  , style "left" <| fromFloat (pos.x - C.topicW2) ++ "px"
  , style "top" <| fromFloat (pos.y - C.topicH2) ++ "px"
  ]


detailTextStyle : Id -> BoxId -> Model -> List (Attribute Msg)
detailTextStyle topicId boxId model =
  let
    r = fromInt C.topicRadius ++ "px"
  in
  [ style "font-size" <| fromInt C.contentFontSize ++ "px"
  , style "width" <| fromFloat C.topicDetailMaxWidth ++ "px"
  , style "line-height" <| fromFloat C.topicLineHeight
  , style "padding" <| fromInt C.topicDetailPadding ++ "px"
  , style "border-radius" <| "0 " ++ r ++ " " ++ r ++ " " ++ r
  ]
  ++ topicBorderStyle topicId boxId model
  ++ selectionStyle topicId boxId model


detailTextViewStyle : List (Attribute Msg)
detailTextViewStyle =
  [ style "min-width" <| fromFloat (C.topicSize.w - C.topicSize.h) ++ "px"
  , style "max-width" "max-content"
  , style "white-space" "pre-wrap"
  , style "pointer-events" "none"
  ]


detailTextEditStyle : Id -> BoxId -> Model -> List (Attribute Msg)
detailTextEditStyle topicId boxId model =
  let
    height = case Box.topicSize topicId boxId model of
      Just size -> size.h
      Nothing -> 0
  in
  [ style "position" "relative"
  , style "top" <| fromFloat -C.topicBorderWidth ++ "px"
  , style "height" <| fromFloat height ++ "px"
  , style "font-family" C.mainFont -- <textarea> default is "monospace"
  , style "border-color" "black" -- <textarea> default is some lightgray
  , style "resize" "none"
  ]


blackBoxTopic : TopicInfo -> TopicProps -> BoxPath -> Model -> TopicRendering
blackBoxTopic topic props boxPath model =
  let
    boxId = Box.firstId boxPath
  in
  ( topicPosStyle props
  , [ div
      (topicFlexboxStyle topic props boxId model
        ++ blackBoxStyle
      )
      (labelTopicHtml topic props boxId model
        ++ boxItemCount topic.id props model
      )
    , div
      (ghostTopicStyle topic boxId model)
      []
    ]
  )


whiteBoxTopic : TopicInfo -> TopicProps -> BoxPath -> Model -> TopicRendering
whiteBoxTopic topic props boxPath model =
  let
    (style, children) = labelTopic topic props boxPath model
  in
  ( style
  , children
    ++ boxItemCount topic.id props model
    ++ [ view topic.id boxPath model ]
  )


unboxedTopic : TopicInfo -> TopicProps -> BoxPath -> Model -> TopicRendering
unboxedTopic topic props boxPath model =
  let
    (style, children) = labelTopic topic props boxPath model
  in
  ( style
  , children
    ++ boxItemCount topic.id props model
  )


boxItemCount : Id -> TopicProps -> Model -> List (Html Msg)
boxItemCount topicId props model =
  let
    itemCount =
      case props.displayMode of
        TopicD _ -> 0
        BoxD _ ->
          case Box.byIdOrLog topicId model.boxes of
            Just box -> box.items |> Dict.values |> List.filter Box.isVisible |> List.length
            Nothing -> 0
  in
  [ div
      itemCountStyle
      [ text <| fromInt itemCount ]
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
    Drag DraftAssoc scrollPos _ boxPath origPos pos _ ->
      let
        pagePos = Point
          (pos.x + scrollPos.x)
          (pos.y + scrollPos.y - C.appHeaderHeight)
      in
      case Box.firstId boxPath == boxId of
        True -> [ lineFunc origPos (relPos pagePos boxPath model) Nothing boxId model ]
        False -> []
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
  case Box.byIdOrLog boxId model.boxes of
    Just box -> Point
      (posAcc.x - box.rect.x1)
      (posAcc.y - box.rect.y1)
    Nothing -> Point 0 0 -- error is already logged



-- STYLE


topicFlexboxStyle : TopicInfo -> TopicProps -> BoxId -> Model -> List (Attribute Msg)
topicFlexboxStyle topic props boxId model =
  let
    r12 = fromInt C.topicRadius ++ "px"
    r34 = case props.displayMode of
      BoxD WhiteBox -> "0"
      _ -> r12
  in
  [ style "display" "flex"
  , style "align-items" "center"
  , style "gap" "8px"
  , style "width" <| fromFloat C.topicSize.w ++ "px"
  , style "height" <| fromFloat C.topicSize.h ++ "px"
  , style "border-radius" <| r12 ++ " " ++ r12 ++ " " ++ r34 ++ " " ++ r34
  ]
  ++ topicBorderStyle topic.id boxId model


topicPosStyle : TopicProps -> List (Attribute Msg)
topicPosStyle { pos } =
  [ style "left" <| fromFloat (pos.x - C.topicW2) ++ "px"
  , style "top" <| fromFloat (pos.y - C.topicH2) ++ "px"
  ]


topicIconBoxStyle : TopicProps -> List (Attribute Msg)
topicIconBoxStyle props =
  let
    r1 = fromInt C.topicRadius ++ "px"
    r4 = case props.displayMode of
      BoxD WhiteBox -> "0"
      _ -> r1
  in
  [ style "flex" "none"
  , style "width" <| fromFloat C.topicSize.h ++ "px"
  , style "height" <| fromFloat C.topicSize.h ++ "px"
  , style "border-radius" <| r1 ++ " 0 0 " ++ r4
  , style "background-color" "black"
  , style "pointer-events" "none"
  ]


detailTopicIconBoxStyle : List (Attribute Msg)
detailTopicIconBoxStyle =
  -- icon box correction as detail topic has no border, in contrast to label topic
  [ style "padding-left" <| fromFloat C.topicBorderWidth ++ "px"
  , style "width" <| fromFloat (C.topicSize.h - C.topicBorderWidth) ++ "px"
  ]


topicLabelStyle : List (Attribute Msg)
topicLabelStyle =
  [ style "font-size" <| fromInt C.contentFontSize ++ "px"
  , style "font-weight" C.topicLabelWeight
  , style "overflow" "hidden"
  , style "text-overflow" "ellipsis"
  , style "white-space" "nowrap"
  , style "pointer-events" "none"
  ]


topicInputStyle : List (Attribute Msg)
topicInputStyle =
  [ style "font-family" C.mainFont -- Default for <input> is "-apple-system" (on Mac)
  , style "font-size" <| fromInt C.contentFontSize ++ "px"
  , style "font-weight" C.topicLabelWeight
  , style "width" "100%"
  , style "position" "relative"
  , style "left" "-4px"
  , style "pointer-events" "initial"
  ]


blackBoxStyle : List (Attribute Msg)
blackBoxStyle =
  [ style "pointer-events" "none" ]


ghostTopicStyle : TopicInfo -> BoxId -> Model -> List (Attribute Msg)
ghostTopicStyle topic boxId model =
  [ style "position" "absolute"
  , style "left" <| fromInt C.blackBoxOffset ++ "px"
  , style "top" <| fromInt C.blackBoxOffset ++ "px"
  , style "width" <| fromFloat C.topicSize.w ++ "px"
  , style "height" <| fromFloat C.topicSize.h ++ "px"
  , style "border-radius" <| fromInt C.topicRadius ++ "px"
  , style "pointer-events" "none"
  , style "z-index" "-1" -- behind topic
  ]
  ++ topicBorderStyle topic.id boxId model
  ++ selectionStyle topic.id boxId model


itemCountStyle : List (Attribute Msg)
itemCountStyle =
  [ style "font-size" <| fromInt C.contentFontSize ++ "px"
  , style "position" "absolute"
  , style "left" "calc(100% + 12px)"
  ]


topicBorderStyle : Id -> BoxId -> Model -> List (Attribute Msg)
topicBorderStyle id boxId model =
  let
    isTarget_ = isTarget id boxId
    targeted = case model.mouse.dragState of
      -- can't move a topic to a box where it is already
      -- can't create assoc when both topics are in different box
      Drag DragTopic _ _ (boxId_ :: _) _ _ target -> isTarget_ target && boxId_ /= id
      Drag DraftAssoc _ _ (boxId_ :: _) _ _ target -> isTarget_ target && boxId_ == boxId
      _ -> False
  in
  [ style "border-width" <| fromFloat C.topicBorderWidth ++ "px"
  , style "border-style" <| if targeted then "dashed" else "solid"
  , style "box-sizing" "border-box"
  , style "background-color" "white"
  ]


selectionStyle : Id -> BoxId -> Model -> List (Attribute Msg)
selectionStyle topicId boxId model =
  case Sel.isSelected topicId boxId model of
    True -> [ style "box-shadow" "gray 5px 5px 5px" ]
    False -> []


isTarget : Id -> BoxId -> Maybe (Id, BoxPath) -> Bool
isTarget topicId boxId target =
  case target of
    Just (targetId, targetBoxPath) ->
      case targetBoxPath of
        targetBoxId :: _ -> topicId == targetId && boxId == targetBoxId
        [] -> False
    Nothing -> False


-- One possible lineFunc
directLine : Point -> Point -> Maybe AssocInfo -> BoxId -> Model -> Svg Msg
directLine pos1 pos2 assoc boxId model =
  line
    ( [ x1 <| fromFloat pos1.x
      , y1 <| fromFloat pos1.y
      , x2 <| fromFloat pos2.x
      , y2 <| fromFloat pos2.y
      ] ++ lineStyle assoc boxId model
    )
    []


-- One possible lineFunc
taxiLine : Point -> Point -> Maybe AssocInfo -> BoxId -> Model -> Svg Msg
taxiLine pos1 pos2 assoc boxId model =
  if abs (pos2.x - pos1.x) < 2 * C.assocRadius then -- straight vertical
    let
      xm = (pos1.x + pos2.x) / 2
    in
    path
      ( [ d ("M " ++ fromFloat xm ++ " " ++ fromFloat pos1.y ++ " V " ++ fromFloat pos2.y)
        ] ++ lineStyle assoc boxId model
      )
      []
  else if abs (pos2.y - pos1.y) < 2 * C.assocRadius then -- straight horizontal
    let
      ym = (pos1.y + pos2.y) / 2
    in
    path
      ( [ d ("M " ++ fromFloat pos1.x ++ " " ++ fromFloat ym ++ " H " ++ fromFloat pos2.x)
        ] ++ lineStyle assoc boxId model
      )
      []
  else -- 5 segment taxi line
    let
      sx = if pos2.x > pos1.x then 1 else -1 -- sign x
      sy = if pos2.y > pos1.y then -1 else 1 -- sign y
      ym = (pos1.y + pos2.y) / 2 -- y mean
      x1 = fromFloat (pos1.x + sx * C.assocRadius)
      x2 = fromFloat (pos2.x - sx * C.assocRadius)
      y1 = fromFloat (ym + sy * C.assocRadius)
      y2 = fromFloat (ym - sy * C.assocRadius)
      sweep1 =
        if sy == 1 then
          if sx == 1 then 1 else 0
        else
          if sx == 1 then 0 else 1
      sweep2 = 1 - sweep1
      sw1 = fromInt sweep1
      sw2 = fromInt sweep2
      r = fromFloat C.assocRadius
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
        ] ++ lineStyle assoc boxId model
      )
      []


lineStyle : Maybe AssocInfo -> BoxId -> Model -> List (Attribute Msg)
lineStyle assoc boxId model =
  let
    color =
      case assoc of
        Just {id} -> if isLimboAssoc id boxId model then C.limboColor else C.assocColor
        Nothing -> C.limboColor
  in
  [ stroke color
  , strokeWidth <| fromFloat C.assocWidth ++ "px"
  , strokeDasharray <| lineDasharray assoc
  , fill "none"
  ]


lineDasharray : Maybe AssocInfo -> String
lineDasharray assoc =
  case assoc of
    Just {itemType} ->
      case itemType of
        "dmx.association" -> "5 0" -- solid
        "dmx.composition" -> "5" -- dotted
        _ -> "1" -- error
    Nothing -> "5 0" -- solid
