module TopicList.TopicList exposing (view, targets, listSize, dragStart, drag, dragStop, init,
  addTopic)

import Array exposing (Array)
import Box
import Config as C
import Dict
import Env exposing (Env2)
import Feature.Mouse as Mouse
import Feature.MouseDef exposing (DragState(..))
import Feature.Text as Text
import Feature.Tool as Tool
import Model exposing (Model, Msg)
import ModelBase exposing (..)
import Shared.Events as Events
import Shared.ViewBase as VB
import Topic
import TopicList.TopicListDef exposing (BoxProps, DragState(..))
import TopicMap.ViewModel exposing (toLocalPos)
import Utils as U

import Html exposing (Html, div, ul, li, text)
import Html.Attributes exposing (style)
import String exposing (fromInt, fromFloat)



type alias HtmlList = List (Html Msg)



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
        topicOrder
        []
        (viewListItem boxPath_)
        (viewList env)
        model
      )
      ++
      viewDraggingTopic boxPath_ model
    )


-- Box.Transform
topicOrder : BoxId -> Model -> List TopicId -> List TopicId
topicOrder boxId model topicIds =
  case byId boxId model of
    Just {order} ->
      order
        |> List.filterMap
          (\orderTopicId ->
            if List.member orderTopicId topicIds then
              Just orderTopicId
            else
              Nothing
          )
    Nothing -> U.fail "TopicList.TopicList.topicOrder" boxId topicIds


-- Box.LevelComplete (env is applied already)
viewList : Env2 -> BoxPath -> HtmlList -> HtmlList
viewList env boxPath topics =
  [ ul
      []
      topics
  ]
  ++ Tool.viewToolbar boxPath env


-- Box.Acc (ixBoxPath is applied already)
viewListItem : BoxPath -> Topic -> BoxPath -> HtmlList -> Maybe HtmlList -> Model -> HtmlList
viewListItem ixBoxPath topic boxPath acc childrenAcc model =
  acc ++
    [ li
        ( Events.draggable topic.id boxPath ixBoxPath
          {- ++ Events.itemClickHandler (T topic.id) boxPath -} -- TODO
          ++ VB.selectionStyle topic.id boxPath model
          ++ listItemStyle topic.id boxPath model
          ++ hoverStyle topic.id boxPath model
        )
        ( [ viewTopic topic boxPath model ]
          ++
          case childrenAcc of
            Just children -> children
            Nothing -> []
        )
    ]


viewTopic : Topic -> BoxPath -> Model -> Html Msg
viewTopic topic boxPath model =
  if Text.isEdit topic.id boxPath model then
    Text.viewInput topic boxPath inputStyle
  else
    text (Topic.label topic)


viewDraggingTopic : BoxPath -> Model -> HtmlList
viewDraggingTopic viewBoxPath model =
  case (model.mouse.dragState, model.topicList.dragState) of
    (DragStarted topicId _ ixBoxPath _, Drag elemPos _) ->
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


listItemStyle : TopicId -> BoxPath -> Model -> Attrs Msg
listItemStyle topicId boxPath model =
  if Mouse.isDragging topicId boxPath model then
    [ style "background-color" "white"
    , style "filter" C.topicLimboFilter
    ]
  else
    []


hoverStyle : TopicId -> BoxPath -> Model -> Attrs Msg
hoverStyle topicId boxPath model =
  if Mouse.isHovered topicId boxPath model then
    [ style "background-color" "orange" ] -- debugging
  else
    []


inputStyle : Attrs Msg
inputStyle =
  [ style "font-family" C.mainFont -- Default for <input> is "-apple-system" (on Mac)
  , style "font-size" <| fromInt C.contentFontSize ++ "px"
  , style "width" "100%"
  , style "position" "relative"
  , style "left" "-4px"
  ]



-- MOUSE


-- ExtManager.NestingDragStart
dragStart : Env2 -> (Model, Cmd Msg)
dragStart {model} =
  ( case model.mouse.dragState of
      DragStarted _ _ ixBoxPath startPos ->
        model
          |> setDragState (Drag (toElemPos startPos ixBoxPath model) startPos)
      _ ->
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
    x = 40 + 40 * level
    y = index * (C.listItemHeight + 1) + 14
    _ = U.info "TopicList.TopicList.toElemPos" index
  in
  Point x y


{-| Transforms a box-local position into an index (for "targets" array).
-}
toIndex : Point -> Int
toIndex localPos =
  (localPos.y - 1) // (C.listItemHeight + 1)


-- ExtManager.NestingDrag
drag : Point -> Env2 -> (Model, Cmd Msg)
drag pos {model} =
  ( case (model.mouse.dragState, model.topicList.dragState) of
      (DragStarted _ _ _ _, Drag elemPos lastPos) ->
        let
          newElemPos = Point
            elemPos.x
            (elemPos.y + pos.y - lastPos.y)
        in
        -- update elemPos and lastPos
        model
          |> setDragState (Drag newElemPos pos)
      _ ->
        let
          _ = U.logError "TopicList.TopicList.drag" "Unexpected drag state"
            (model.mouse.dragState, model.topicList.dragState)
        in
        model
  , Cmd.none
  )


-- ExtManager.NestingDragStop
dragStop : Env2 -> (Model, Cmd Msg)
dragStop {model} =
  (model, Cmd.none)


setDragState : DragState -> Model -> Model
setDragState dragState ({topicList} as model) =
  { model | topicList = { topicList | dragState = dragState }}



-- MODEL


{- The content of the given box as an array of targets -}
targets : BoxPath -> Model -> Array (Int, Target) -- Int: nesting level
targets boxPath model =
  targets_ 0 boxPath model


targets_ : Int -> BoxPath -> Model -> Array (Int, Target) -- Int: nesting level
targets_ level boxPath model =
  let
    boxId = Box.firstId boxPath
  in
  Box.topicIds boxId model |> List.foldl -- TODO: respect order, utilize Box.traverseWith
    (\topicId acc ->
      let
        t = Array.push (level, (T topicId, boxPath)) acc
      in
      if Topic.isBox topicId model then
        Array.append t (targets_ (level + 1) (BoxId topicId :: boxPath) model) -- recursion
      else
        t
    )
    Array.empty


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


updateOrder : BoxId -> (List TopicId -> List TopicId) -> Model -> Model
updateOrder boxId transform ({topicList} as model) =
  { model | topicList =
    { topicList | boxProps = topicList.boxProps |> Dict.update (toBoxId boxId)
        (\maybeViewProps ->
          case maybeViewProps of
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
