module TopicList.Mouse exposing (dragStart, drag, updateDropTarget, dragStop)

import Box
import Config as C
import Console
import Env exposing (Env)
import Extension
import Feature.Sel as Sel
import Model exposing (Model, Msg)
import ModelBase exposing (..)
import Outcome exposing (..)
import Topic
import TopicList.TopicList as TopicList
import TopicList.TopicListDef exposing (DropMode(..))
import TopicMap.Mouse
import TopicMap.TopicMap as TopicMap

import Array



-- Dispatch.ExtDragStart
dragStart : Env -> (Model, Cmd Msg)
dragStart {model} =
  ( case model.mouse.dragSource of
      Just {ixBoxPath, startPos} ->
        model
          |> setDragPos (Just (toElemPos startPos ixBoxPath model))
      Nothing ->
        let
          _ = Console.logError "TopicList.Mouse.dragStart" "Unexpected drag state"
            model.mouse.dragSource
        in
        model
  , Cmd.none
  )


toElemPos : Point -> BoxPath -> Model -> Point
toElemPos clientPos ixBoxPath model =
  let
    localPos = TopicMap.toLocalPos clientPos ixBoxPath model
    index = toIndex localPos
    level =
      case Array.get index (TopicList.targets ixBoxPath model) of
        Just (level_, _) -> level_
        Nothing -> 0
    x = 41 + 41 * level -- 41 = 40 + 1px item border
    y = index * (C.listItemHeight + 4) - level + 16 -- 4 = 2px border (top/bottom) + 2px browser
    _ = Console.info "TopicList.Mouse.toElemPos" (index, localPos)
  in
  Point x y


{-| Transforms a box-local position into an index (for "targets" array).
-}
toIndex : Point -> Int
toIndex localPos =
  (localPos.y - 13) // (C.listItemHeight + 4)


-- Dispatch.ExtDrag
drag : Point -> Env -> (Model, Cmd Msg)
drag clientPos {model} =
  ( case (model.mouse.dragSource, model.topicList.dragPos) of
      (Just {lastPointerPos}, Just dragPos) ->
        model
          |> setDragPos
            (Just
              { dragPos
              | x = dragPos.x + clientPos.x - lastPointerPos.x
              , y = dragPos.y + clientPos.y - lastPointerPos.y
              }
            )
      _ ->
        let
          _ = Console.logError "TopicList.Mouse.drag" "Unexpected drag state"
            (model.mouse.dragSource, model.topicList.dragPos)
        in
        model
  , Cmd.none
  )


-- Dispatch.ExtDropTargeting
updateDropTarget : Point -> Env -> (Model, Maybe Target)
updateDropTarget clientPos {model} =
  if TopicMap.Mouse.isDraftAssoc model then
    (model, acceptAssoc model)
  else
    case dropTargetAt clientPos model of
      Just (dropTarget, dropMode) ->
        ( model
            |> setDropMode (Just dropMode)
        , Just dropTarget
        )
      Nothing -> (model, Nothing)


acceptAssoc : Model -> Maybe Target
acceptAssoc model =
  case (model.mouse.hover, model.mouse.dragSource) of
    (Just {ixBoxPath}, Just {boxPath}) ->
      case ixBoxPath of
        BoxId topicId :: targetPath ->
          if targetPath == boxPath then
            Just (T topicId, targetPath)
          else
            Nothing
        _ -> Console.logError "TopicList.Mouse.acceptAssoc" "Empty ixBoxPath" Nothing
    _ ->
      let
        _ = Console.logError "TopicList.Mouse.acceptAssoc" "Unexpected mouse state"
          (model.mouse.hover, model.mouse.dragSource)
      in
      Nothing


{-| Checks if the currently hovered list item is accepted as a drop target. If so the drop
target is returned, along with the drop mode (drop vs. insert). Acceptance depends on a cycle
check. Drop mode depends on vertical mouse position within the hovered list item (upper half
vs. lower half).
-}
dropTargetAt : Point -> Model -> Maybe (Target, DropMode)
dropTargetAt clientPos model =
  case (model.mouse.hover, model.mouse.dragSource) of
    (Just {ixBoxPath, target}, Just {topicId}) ->
      case (ixBoxPath, target) of
        (ixBoxId :: _, (T dropTopicId, dropBoxId :: _)) ->
          let
            localPos = TopicMap.toLocalPos clientPos ixBoxPath model
            lowerHalf =
              modBy (C.listItemHeight + 4) (localPos.y - 13) > (C.listItemHeight + 4) // 2
            isContentHovered = TopicList.isContentHovered ixBoxId localPos model
            (dropMode, targetBoxId) =
              if lowerHalf || not isContentHovered then
                (Drop, dropTopicId)
              else
                (InsertBefore, fromBoxId dropBoxId)
            isCyclic = Box.hadDeepTopic targetBoxId topicId model
            -- _ = Console.info "TopicList.Mouse.dropTargetAt" (targetBoxId, dropMode, localPos)
          in
          if not isCyclic then
            Just (target, dropMode)
          else
            Nothing
        _ ->
          Nothing -- TODO: error?
    _ ->
      let
        _ = Console.logError "TopicList.Mouse.dropTargetAt" "Unexpected mouse state"
          (model.mouse.hover, model.mouse.dragSource)
      in
      Nothing


-- Dispatch.ExtDragStop
dragStop : Env -> Outcome
dragStop ({model} as env) =
  let
    noOp = Outcome.default model
    outcome =
      case model.mouse.dragSource of
        Just {topicId, boxPath} ->
          case model.mouse.dropTarget of
            Just (T targetTopicId, targetBoxId :: _) ->
              env
                |> processDrop topicId (Box.firstId boxPath) targetTopicId targetBoxId
            Nothing ->
              let
                _ = Console.info "TopicList.Mouse.dragStop" "no drop target -> select topic"
              in
              env
                |> Env.map (Sel.select (T topicId) boxPath)
                |> Env.outcome
            _ ->
              Console.logError "TopicList.Mouse.dragStop"
                (Console.toString model.mouse.dropTarget) noOp
        Nothing ->
          Console.logError "TopicList.Mouse.dragStop"
            (Console.toString model.mouse.dragSource) noOp
  in
  outcome
    |> Outcome.map (setDragPos Nothing)
    |> Outcome.map (setDropMode Nothing)


processDrop : TopicId -> BoxId -> TopicId -> BoxId -> Env -> Outcome
processDrop sourceTopicId sourceBoxId targetTopicId targetBoxId ({model} as env) =
  let
    _ = Console.info "TopicList.Mouse.processDrop"
      { sourceTopicId = sourceTopicId
      , sourceBoxId = sourceBoxId
      , targetTopicId = targetTopicId
      , targetBoxId = targetBoxId
      , dropMode = model.topicList.dropMode
      }
    --
    addTopic : Env -> Env
    addTopic env_ =
      case env_.model.topicList.dropMode of
        Just Drop ->
          env_
            |> Box.turnTopicIntoBox targetTopicId Extension.TopicList
            |> addTopic_ sourceTopicId (BoxId targetTopicId)
            |> Env.autoSize
        Just InsertBefore ->
          env_
            |> addTopic_ sourceTopicId targetBoxId
            |> Env.map (TopicList.reorderTopic sourceTopicId targetBoxId targetTopicId)
            |> Env.autoSize
        Nothing ->
          let
            _ = Console.logError "TopicList.Mouse.processDrop" "Unexpected dropMode" Nothing
          in
          env_
  in
  env
    |> Env.map (Box.removeTopic sourceTopicId sourceBoxId)
    |> addTopic
    |> Env.outcomeDir (Directives Store Push) -- FIXME: Push only if not foreign drop


addTopic_ : TopicId -> BoxId -> Env -> Env
addTopic_ topicId boxId env =
  let
    setBoxRenderer : Env -> Env
    setBoxRenderer ({model} as env_) =
      if Topic.isBox topicId model then
        env_
          |> Env.map (Box.setRenderer (BoxId topicId) Extension.TopicList)
      else
        env_
  in
  env
    |> Box.addTopic (BoxTopic topicId Expanded) boxId
    |> setBoxRenderer


setDragPos : Maybe Point -> Model -> Model
setDragPos dragPos ({topicList} as model) =
  { model | topicList = { topicList | dragPos = dragPos }}


setDropMode : Maybe DropMode -> Model -> Model
setDropMode dropMode ({topicList} as model) =
  -- let
  --   _ = Console.info "TopicList.Mouse.setDropMode" dropMode
  -- in
  { model | topicList = { topicList | dropMode = dropMode }}
