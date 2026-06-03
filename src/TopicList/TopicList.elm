module TopicList.TopicList exposing (view, targets, listSize, dragStart, drag, dragStop, init,
  addTopic)

import Box
import Config as C
import Dict
import Env exposing (Env2)
import Feature.Mouse as Mouse
import Feature.Text as Text
import Feature.Tool as Tool exposing (ToolbarPos)
import Model exposing (Model, Msg(..))
import ModelBase exposing (..)
import Outcome exposing (..)
import Shared.Events as Events
import Shared.ViewBase as VB
import Topic
import TopicList.TopicListDef exposing (BoxProps, DragState, DropTarget(..), Targets)
import TopicMap.ViewModel exposing (toLocalPos)
import Utils as U

import Array
import Html exposing (Html, div, ul, li, text)
import Html.Attributes exposing (style)
import String exposing (fromInt)



type alias HtList = List (Html Msg)



-- VIEW


-- ExtManager.NestingBoxRenderer
-- For the fullscreen box boxPath is empty
view : BoxId -> BoxPath -> Env2 -> Html Msg
view boxId boxPath ({model} as env) =
  let
    boxPath_ = boxId :: boxPath
  in
  div
    ( listStyle boxId boxPath model )
    ( (Box.traverseWith
        boxPath_
        listOrder
        []
        (viewListItem boxPath_)
        (viewList boxPath_ env)
        model
      )
      ++
      viewDraggingTopic boxPath_ model
    )


listStyle : BoxId -> BoxPath -> Model -> Attrs Msg
listStyle boxId boxPath model =
  let
    size = listSize boxId model
  in
  VB.boxStyle boxId size boxPath model
    ++ listFontStyle


listFontStyle : Attrs Msg
listFontStyle =
  [ style "font-family" C.mainFont
  , style "font-size" <| fromInt C.contentFontSize ++ "px"
  , style "line-height" <| fromInt C.listItemHeight ++ "px"
  ]


-- Box.Transform
{- Applies list order to box content. Iterates over list's order list and filters those TopicIds
contained in the box. Note: order list might be longer than box's content list (maintaining
stability).
-}
listOrder : BoxId -> Model -> List TopicId -> List TopicId
listOrder boxId model topicIds =
  let
    isBoxContent : TopicId -> Maybe TopicId
    isBoxContent topicId =
      if List.member topicId topicIds then
        Just topicId
      else
        Nothing
  in
  case byId boxId model of
    Just {order} -> order
      |> List.filterMap isBoxContent
    Nothing -> U.fail "TopicList.TopicList.listOrder" boxId topicIds


-- Box.LevelDone (ixBoxPath and env are applied already)
viewList : BoxPath -> Env2 -> BoxPath -> HtList -> HtList
viewList ixBoxPath ({model} as env) boxPath topics =
  [ ul
      []
      topics
  ]
  ++ Tool.viewToolbar boxPath (toolbarPos boxPath ixBoxPath model) env


-- Box.Acc (ixBoxPath is applied already)
viewListItem : BoxPath -> Topic -> Level -> BoxPath -> HtList -> Maybe HtList -> Model -> HtList
viewListItem ixBoxPath topic level boxPath acc childrenAcc model =
  acc ++
    [ li
        ( Events.draggable topic.id boxPath ixBoxPath
          -- ++ Events.itemClickHandler (T topic.id) boxPath -- TODO
          ++ VB.selectionStyle topic.id boxPath model
          ++ listItemStyle topic.id boxPath model
          ++ topicBorderStyle topic.id boxPath model
          -- ++ hoverStyle topic.id boxPath model -- debug
        )
        ( viewTopic topic boxPath model
          ++
          case childrenAcc of
            Just children -> children
            Nothing -> []
        )
    ]


listItemStyle : TopicId -> BoxPath -> Model -> Attrs Msg
listItemStyle topicId boxPath model =
  if Mouse.isTopicDragging topicId boxPath model then
    [ style "background-color" "white"
    , style "filter" C.topicLimboFilter
    ]
  else
    []


topicBorderStyle : TopicId -> BoxPath -> Model -> Attrs Msg
topicBorderStyle topicId boxPath model =
  let
    isTarget_ = isTarget (Drop (T topicId, boxPath)) model
  in
  [ style "border-width" <| fromInt C.topicBorderWidth ++ "px"
  , style "border-color" <| if isTarget_ then "black" else "transparent"
  , style "border-style" "dashed"
  ]


-- for hit-test debugging
hoverStyle : TopicId -> BoxPath -> Model -> Attrs Msg
hoverStyle topicId boxPath model =
  if Mouse.isHovered topicId boxPath model then
    [ style "background-color" "orange" ]
  else
    []


viewTopic : Topic -> BoxPath -> Model -> HtList
viewTopic topic boxPath model =
  [ div
      (insertionPointStyle topic.id boxPath model)
      []
  , div
      []
      [ if Text.isEdit topic.id boxPath model then
          Text.viewInput topic boxPath inputStyle
        else
          text (Topic.label topic)
      ]
  ]


insertionPointStyle : TopicId -> BoxPath -> Model -> Attrs Msg
insertionPointStyle topicId boxPath model =
  let
    isTarget_ = isTarget (InsertBefore (T topicId, boxPath)) model
  in
  [ style "height" "2px" ]
  ++
  if isTarget_ then
    [ style "background-color" "black" ]
  else
    []


isTarget : DropTarget -> Model -> Bool
isTarget dropTarget_ model =
  case model.topicList.dragState of
    Just {dropTarget} -> dropTarget == Just dropTarget_
    _ -> False


viewDraggingTopic : BoxPath -> Model -> HtList
viewDraggingTopic viewBoxPath model =
  case (model.mouse.dragState, model.topicList.dragState) of
    (Just {topicId, ixBoxPath}, Just {elemPos}) ->
      case (viewBoxPath == ixBoxPath, Topic.fromId topicId model) of
        (True, Just topic) ->
          [ div
              (draggingTopicStyle elemPos)
              [text (Topic.label topic)]
          ]
        _ -> []
    _ -> []


draggingTopicStyle : Point -> Attrs Msg
draggingTopicStyle pos =
  [ style "position" "absolute"
  , style "left" <| fromInt pos.x ++ "px"
  , style "top" <| fromInt pos.y ++ "px"
  ]


inputStyle : Attrs Msg
inputStyle =
  [ style "font-family" C.mainFont -- Default for <input> is "-apple-system" (on Mac)
  , style "font-size" <| fromInt C.contentFontSize ++ "px"
  , style "width" "100%"
  , style "position" "relative"
  , style "left" "-4px"
  ]


-- Toolbar

toolbarPos : BoxPath -> BoxPath -> Model -> ToolbarPos
toolbarPos boxPath ixBoxPath model =
  ToolbarPos
    (\topic ->
      case targets ixBoxPath model |> findIndexOf (T topic.id, boxPath) of
        Just (index, level) ->
          Point
            (40 + 40 * level)
            (index * (C.listItemHeight + 4) - 16)
        Nothing -> Point 0 0
    )
    (\assoc -> Point 0 0)


findIndexOf : Target -> Targets -> Maybe (Int, Level)
findIndexOf target targets_ =
  let
    found =
      targets_
        |> Array.toIndexedList
        |> List.filter (\(_, (_, t)) -> t == target)
  in
  case found of
    [(index, (level, _))] -> Just (index, level)
    [] -> U.logError "TopicList.Geometry.findIndexOf"
      ("Target " ++ U.toString target ++ " not found")
      Nothing
    _ -> U.logError "TopicList.Geometry.findIndexOf"
      ("Target " ++ U.toString target ++ " found " ++ fromInt (List.length found) ++ " times")
      Nothing



-- MOUSE


-- ExtManager.NestingDragStart
dragStart : Env2 -> (Model, Cmd Msg)
dragStart {model} =
  ( case model.mouse.dragState of
      Just {ixBoxPath, startPos} ->
        let
          elemPos = toElemPos startPos ixBoxPath model
        in
        model
          |> setDragState (Just (DragState elemPos startPos Nothing))
      Nothing ->
        let
          _ = U.logError "TopicList.TopicList.dragStart" "Unexpected drag state"
            model.mouse.dragState
        in
        model
  , Cmd.none
  )


toElemPos : Point -> BoxPath -> Model -> Point
toElemPos clientPos ixBoxPath model =
  let
    index = toIndex (toLocalPos clientPos ixBoxPath model)
    level =
      case Array.get index (targets ixBoxPath model) of
        Just (level_, _) -> level_
        Nothing -> 0
    x = 41 + 41 * level -- 41 = 40 + 1px item border
    y = index * (C.listItemHeight + 4) - level + 16 -- 4 = 2px border (top/bottom) + 2px browser
    _ = U.info "TopicList.TopicList.toElemPos" index
  in
  Point x y


{-| Transforms a box-local position into an index (for "targets" array).
-}
toIndex : Point -> Int
toIndex localPos =
  (localPos.y - 1) // (C.listItemHeight + 4)


-- ExtManager.NestingDrag
drag : Point -> Env2 -> (Model, Cmd Msg)
drag clientPos {model} =
  ( case (model.mouse.dragState, model.topicList.dragState) of
      (Just {topicId, ixBoxPath}, Just ({elemPos, lastPos} as dragState)) ->
        let
          localPos = toLocalPos clientPos ixBoxPath model
        in
        model
          |> setDragState
            (Just
              { dragState |
                elemPos = { elemPos | y = elemPos.y + clientPos.y - lastPos.y }
              , lastPos = clientPos
              , dropTarget = dropTargetAt localPos topicId model
              }
            )
      _ ->
        let
          _ = U.logError "TopicList.TopicList.drag" "Unexpected drag state"
            (model.mouse.dragState, model.topicList.dragState)
        in
        model
  , Cmd.none
  )


dropTargetAt : Point -> TopicId -> Model -> Maybe DropTarget
dropTargetAt localPos dragTopicId model =
  case model.mouse.hover of
    Just ((T dropTopicId, dropBoxId :: _) as target) ->
      let
        lowerHalf = modBy (C.listItemHeight + 4) (localPos.y - 1) > (C.listItemHeight + 4) // 2
        (dropMode, targetBoxId) =
          case lowerHalf of
            True -> (Drop, dropTopicId)
            False -> (InsertBefore, fromBoxId dropBoxId)
        isCyclic = Box.hadDeepTopic targetBoxId dragTopicId model
      in
      if not isCyclic then
        Just (dropMode target)
      else
        Nothing
    _ -> Nothing


-- ExtManager.NestingDragStop
dragStop : Env2 -> Outcome
dragStop ({model} as env2) =
  let
    outcome =
      case (model.mouse.dragState, model.topicList.dragState) of
        (Just {topicId, boxPath}, Just dragState) ->
          case dragState.dropTarget of
            Just dropTarget ->
              env2
                |> processDrop topicId (Box.firstId boxPath) dropTarget
            Nothing ->
              let
                _ = U.info "TopicList.TopicList.dragStop" "no drop target -> ItemClicked"
              in
              (Outcome.with
                (U.command <| ItemClicked (T topicId) boxPath)
                model
              )
        _ -> U.logError "TopicList.TopicList.dragStop" (U.toString model.mouse.dragState)
          (Outcome.with Cmd.none model)
  in
  outcome
    |> Outcome.map (setDragState Nothing)


processDrop : TopicId -> BoxId -> DropTarget -> Env2 -> Outcome
processDrop sourceTopicId sourceBoxid dropTarget ({model} as env) =
  let
    _ = U.info "TopicList.TopicList.processDrop"
      { sourceTopicId = sourceTopicId
      , sourceBoxid = sourceBoxid
      , dropTarget = dropTarget
      }
  in
  model
    |> Box.removeTopic sourceTopicId sourceBoxid
    |> \model_ ->
      (case dropTarget of
        Drop (T targetTopicId, _) ->
          let
            boxId = BoxId targetTopicId
          in
          model_
            |> Tool.createBoxOnDemand targetTopicId
            |> Box.addTopic (BoxTopic sourceTopicId Expanded) boxId
            |> init boxId
            |> Env.autoSize2 env
        InsertBefore (T targetTopicId, targetBoxId :: _) ->
          model_
            |> Box.addTopic (BoxTopic sourceTopicId Expanded) targetBoxId
            |> insert sourceTopicId targetBoxId targetTopicId
            |> Env.autoSize2 env
        _ -> U.logError "TopicList.TopicList.processDrop" (U.toString dropTarget) model_
      )
    |> Outcome (Directives Persistent StoreUndo) Cmd.none


{- Inserts a topic ID into the order list at the given insertion point.
-}
insert : TopicId -> BoxId -> TopicId -> Model -> Model
insert topicId boxId beforeTopicId model =
  let
    insertion : TopicId -> (List TopicId, Bool) -> (List TopicId, Bool)
    insertion id (list, found) =
      case (id == beforeTopicId, found) of -- at insertion point? insertion point found already?
        (False, _) -> (id :: list, found)
        (True, False) -> ([topicId, id] ++ list, True)
        (True, True) -> U.logError "TopicList.TopicList.insert"
          "Found more than one insertion point" (list, found)
  in
  model
    |> updateOrder boxId
      (\orderList -> orderList
        |> List.filter (\id -> id /= topicId) -- remove before inserting
        |> List.foldr insertion ([], False)
        |> \(list, found) ->
            case found of
              True -> list
              False -> U.logError "TopicList.TopicList.insert" "Insertion point not found"
                orderList
      )


setDragState : Maybe DragState -> Model -> Model
setDragState dragState ({topicList} as model) =
  { model | topicList = { topicList | dragState = dragState }}



-- MODEL


{- Box content as an array of targets -}
targets : BoxPath -> Model -> Targets
targets boxPath model =
  Box.traverseWith
    boxPath
    listOrder
    Array.empty
    targetAcc
    (\_ levelResult -> levelResult)
    model


-- Box.Acc
targetAcc : Topic -> Level -> BoxPath -> Targets -> Maybe Targets -> Model -> Targets
targetAcc topic level boxPath acc childrenAcc model =
  acc
    |> Array.push (level, (T topic.id, boxPath))
    |> \t -> Array.append t (childrenAcc |> Maybe.withDefault Array.empty)


-- ExtManager.ExtInit
init : BoxId -> Model -> Model
init boxId model =
  model
    |> Box.topicIds boxId
    |> List.foldl
      (\topicId acc ->
        if Topic.isBox topicId acc then
          init (BoxId topicId) acc
        else
          acc
      )
      model
    |> createBoxProps boxId
    |> initOrder boxId


createBoxProps : BoxId -> Model -> Model
createBoxProps boxId ({topicList} as model) =
  let
    id = toBoxId boxId
  in
  if Dict.member id model.topicList.boxProps then
    let
      _ = U.info "TopicList.TopicList.createBoxProps"
        ("Box (" ++ U.toString boxId ++ ") has BoxProps entry already")
    in
    model
  else
    let
      _ = U.info "TopicList.TopicList.createBoxProps"
        ("Creating BoxProps entry for box (" ++ U.toString boxId ++ ")")
    in
    { model | topicList =
      { topicList | boxProps = topicList.boxProps
          |> Dict.insert id (BoxProps boxId [] (Size 0 0))
      }
    }


initOrder : BoxId -> Model -> Model
initOrder boxId ({topicList} as model) =
  model
    |> updateOrder boxId
      (\orderList ->
        let
          missing =
            List.filterMap
              (missingTopicIds orderList)
              (Box.topicIds boxId model)
          _ = U.info "TopicList.TopicList.initOrder"
            ("Add missing BoxProps " ++ U.toString missing ++ " to " ++ U.toString orderList)
        in
        missing ++ orderList
      )


missingTopicIds : List TopicId -> TopicId -> Maybe TopicId
missingTopicIds orderList topicId =
  if List.member topicId orderList then
    Nothing
  else
    Just topicId


-- ExtManager.AddTopic
addTopic : TopicId -> BoxId -> PosHint -> Env2 -> (Model, Cmd Msg)
addTopic _ boxId _ ({model} as env) =
  -- Note: added topic might be nested. Needs to init BoxProps recursively.
  -- We just init entire box.
  ( model
      |> init boxId
      |> Env.autoSize2 env
  , Cmd.none
  )


{- Canonical order list transformation.
Logs an error if the BoxProps entry is missing.
-}
updateOrder : BoxId -> (List TopicId -> List TopicId) -> Model -> Model
updateOrder boxId transform ({topicList} as model) =
  { model | topicList =
    { topicList | boxProps = topicList.boxProps |> Dict.update (toBoxId boxId)
        (\maybeBoxProps ->
          case maybeBoxProps of
            Just boxProps -> Just { boxProps | order = transform boxProps.order }
            Nothing -> U.logError "TopicList.TopicList.updateOrder"
              (U.toString {boxId = boxId}) Nothing
        )
    }
  }


listSize : BoxId -> Model -> Size
listSize boxId model =
  case byId boxId model of
    Just {size} -> size
    Nothing -> U.fail "TopicList.TopicList.listSize" boxId (Size 0 0)


{-| Logs an error if the BoxProps entry is missing.
-}
byId : BoxId -> Model -> Maybe BoxProps
byId boxId model =
  case model.topicList.boxProps |> Dict.get (toBoxId boxId) of
    Just boxProps -> Just boxProps
    Nothing -> U.logError "TopicList.TopicList.byId"
      ("Missing BoxProps entry for (" ++ U.toString boxId ++ ")") Nothing
