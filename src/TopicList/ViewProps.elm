module TopicList.ViewProps exposing (view, listSize, dragStart, drag, dragStop, init, addTopic)

import Box
import Config as C
import Dict
import Env exposing (Env2)
import Feature.Mouse as Mouse
import Feature.Text as Text
import Feature.Tool as Tool
import Model exposing (Model, Msg)
import ModelBase exposing (..)
import Shared.Events as Events
import Shared.ViewBase as VB
import Topic
import TopicList.TopicListDef exposing (ViewProps)
import Utils as U

import Html exposing (Html, div, ul, li, text)
import Html.Attributes exposing (style)
import String exposing (fromInt, fromFloat)



type alias HtmlList = List (Html Msg)



-- VIEW


-- Extension point
-- For the fullscreen box boxPath is empty
view : BoxId -> BoxPath -> Env2 -> Html Msg
view boxId boxPath ({model} as env) =
  div
    (listStyle boxId boxPath model)
    (Box.traverseWith
      (boxId :: boxPath)
      topicOrder
      []
      viewListItem
      (viewList env)
      model
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
    Nothing -> U.fail "TopicList.ViewProps.topicOrder" boxId topicIds


-- Box.Accumulator
viewListItem : Topic -> BoxPath -> HtmlList -> Maybe HtmlList -> Model -> HtmlList
viewListItem topic boxPath acc childrenAcc model =
  acc ++
    [ li
        ( Events.itemClickHandler (T topic.id) boxPath
          ++ VB.selectionStyle topic.id boxPath model
          ++ hoverStyle topic.id boxPath model
        )
        ( [ viewTopic topic boxPath model ]
          ++
          case childrenAcc of
            Just children -> children
            Nothing -> []
        )
    ]


-- Box.LevelComplete
viewList : Env2 -> BoxPath -> HtmlList -> HtmlList
viewList env boxPath topics =
  [ ul
      []
      topics
  ]
  ++ Tool.viewToolbar boxPath env


viewTopic : Topic -> BoxPath -> Model -> Html Msg
viewTopic topic boxPath model =
  if Text.isEdit topic.id boxPath model then
    Text.viewInput topic boxPath inputStyle
  else
    text <| Topic.label topic


hoverStyle : TopicId -> BoxPath -> Model -> Attrs Msg
hoverStyle topicId boxPath model =
  if Mouse.isHovered topicId boxPath model then
    [ style "background-color" "beige" ] -- for debug
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
  , style "line-height" <| fromFloat C.topicLineHeight
  ]



-- EVENTS


-- TODO
dragStart : TopicId -> BoxPath -> Point -> PointerType -> Env2 -> (Model, Cmd Msg)
dragStart topicId boxPath pos pointerType {model} =
  (model, Cmd.none)


-- TODO
drag : Point -> Env2 -> (Model, Cmd Msg)
drag pos {model} =
  (model, Cmd.none)


-- TODO
dragStop : Env2 -> (Model, Cmd Msg)
dragStop {model} =
  (model, Cmd.none)



-- MODEL


-- Extension point
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
    |> createViewProps boxId
    |> initViewProps boxId


createViewProps : BoxId -> Model -> Model
createViewProps boxId ({topicList} as model) =
  let
    id = toBoxId boxId
  in
  if Dict.member id model.topicList.viewProps then
    let
      _ = U.info "TopicList.ViewProps.createViewProps"
        ("box " ++ fromInt id ++ " has ViewProps already")
    in
    model
  else
    let
      _ = U.info "TopicList.ViewProps.createViewProps"
        ("creating ViewProps for box " ++ fromInt id)
    in
    { model | topicList =
      { topicList | viewProps = topicList.viewProps |> Dict.insert id
          (ViewProps boxId [] (Size 0 0))
      }
    }


initViewProps : BoxId -> Model -> Model
initViewProps boxId ({topicList} as model) =
  model
    |> updateOrder boxId
      (\orderList ->
        let
          missing =
            List.filterMap
              (missingTopicIds orderList)
              (Box.topicIds boxId model)
          _ = U.info "TopicList.ViewProps.initViewProps"
            ("add missing ViewProps " ++ U.toString missing ++ " to " ++ U.toString orderList)
        in
        missing ++ orderList
      )


missingTopicIds : List TopicId -> TopicId -> Maybe TopicId
missingTopicIds orderList topicId =
  if List.member topicId orderList then
    Nothing
  else
    Just topicId


-- Extension point
addTopic : TopicId -> BoxId -> PosHint -> Env2 -> (Model, Cmd Msg)
addTopic topicId boxId posHint {model} =
  ( model
      |> updateOrder boxId
        (\orderList -> topicId :: orderList) -- FIXME: check membership first
  , Cmd.none
  )


updateOrder : BoxId -> (List TopicId -> List TopicId) -> Model -> Model
updateOrder boxId transform ({topicList} as model) =
  { model | topicList =
    { topicList | viewProps = topicList.viewProps |> Dict.update (toBoxId boxId)
        (\maybeViewProps ->
          case maybeViewProps of
            Just viewProps -> Just { viewProps | order = transform viewProps.order }
            Nothing -> U.logError "TopicList.ViewProps.updateOrder"
              (U.toString {boxId = boxId}) Nothing
        )
    }
  }


listSize : BoxId -> Model -> Size
listSize boxId model =
  case byId boxId model of
    Just {size} -> size
    Nothing -> U.fail "TopicList.ViewProps.listSize" boxId (Size 0 0)


{-| Logs an error if the ViewProps are missing. -}
byId : BoxId -> Model -> Maybe ViewProps
byId boxId model =
  case model.topicList.viewProps |> Dict.get (toBoxId boxId) of
    Just viewProps -> Just viewProps
    Nothing -> U.logError "TopicList.ViewProps.byId"
      ("Missing ViewProps for " ++ U.toString boxId) Nothing
