module TopicList.TopicList exposing (view, listSize, dragStart, drag, dragStop, init)

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
import TopicList.TopicListDef exposing (TopicList)
import Utils as U

import Html exposing (Html, div, ul, li, text)
import Html.Attributes exposing (style)
import String exposing (fromInt, fromFloat)



-- VIEW


-- For the fullscreen box boxPath is empty
view : BoxId -> BoxPath -> Env2 -> Html Msg
view boxId boxPath ({model} as env) =
  div
    ( listStyle boxId boxPath model )
    ( viewList (boxId :: boxPath) env )


viewList : BoxPath -> Env2 -> List (Html Msg)
viewList boxPath ({model} as env) =
  let
    boxId = Box.firstId boxPath
  in
  [ ul
      []
      ( Box.topics boxId model |> List.map
          (\topic ->
            li
              ( Events.itemClickHandler (T topic.id) boxPath
                ++ VB.selectionStyle topic.id boxPath model
                ++ hoverStyle topic.id boxPath model
              )
              ( [ viewTopic topic boxPath model ]
                ++
                if Topic.isBox topic.id model then
                  viewList (BoxId topic.id :: boxPath) env -- recursion
                else
                  []
              )
          )
      )
  ]
  ++ Tool.viewToolbar boxPath env


hoverStyle : TopicId -> BoxPath -> Model -> Attrs Msg
hoverStyle topicId boxPath model =
  if Mouse.isHovered topicId boxPath model then
    [ style "background-color" "beige" ] -- for debug
  else
    []


viewTopic : Topic -> BoxPath -> Model -> Html Msg
viewTopic topic boxPath model =
  if Text.isEdit topic.id boxPath model then
    Text.viewInput topic boxPath inputStyle
  else
    text <| Topic.label topic


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


init : BoxId -> Model -> Model
init boxId ({topicList} as model) =
  let
    id = toBoxId boxId
  in
  if Dict.member id model.topicList.topicLists then
    model
  else
    { model | topicList =
      { topicList | topicLists = topicList.topicLists |> Dict.insert id
          (TopicList boxId [] (Size 0 0)) -- TOOO: "order" list
      }
    }


listSize : BoxId -> Model -> Size
listSize boxId model =
  case byId boxId model of
    Just {size} -> size
    Nothing -> U.fail "TopicList.TopicList.listSize" boxId (Size 0 0)


{-| Logs an error if the TopicList is missing. -}
byId : BoxId -> Model -> Maybe TopicList
byId boxId model =
  case model.topicList.topicLists |> Dict.get (toBoxId boxId) of
    Just list -> Just list
    Nothing -> U.boxNotFound "TopicList.TopicList.byId" boxId Nothing
