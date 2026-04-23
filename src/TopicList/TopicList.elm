module TopicList.TopicList exposing (view, listSize)

import Box
import Config as C
import Dict
import Env exposing (ExtManager)
import Feature.Mouse as Mouse
import Feature.Text as Text
import Feature.Tool as Tool
import Item
import Model exposing (Model, Msg)
import ModelBase exposing (..)
import ViewBase as VB

import Html exposing (Html, div, ul, li, text)
import Html.Attributes exposing (style)
import String exposing (fromInt, fromFloat)



-- VIEW


-- For the fullscreen box boxPath is empty
view : BoxId -> BoxPath -> ExtManager -> Model -> Html Msg
view boxId boxPath ext model =
  div
    ( listStyle boxId boxPath model )
    ( [ viewList (boxId :: boxPath) model ]
      ++ Tool.viewToolbar (boxId :: boxPath) ext model -- TODO
    )


viewList : BoxPath -> Model -> Html Msg
viewList boxPath model =
  let
    boxId = Box.firstId boxPath
  in
  ul
    []
    ( Box.topics boxId model
        |> List.map
          (\topic ->
            li
              ( Mouse.itemClickHandler topic.id boxPath
                ++
                VB.selectionStyle topic.id boxPath model
              )
              ( [ viewTopic topic boxPath model ]
                ++
                if Item.isBox topic.id model then
                  [ viewList (topic.id :: boxPath) model ]
                else
                  []
              )
          )
    )


viewTopic : TopicInfo -> BoxPath -> Model -> Html Msg
viewTopic topic boxPath model =
  if Text.isEdit topic.id boxPath model then
    Text.viewInput topic boxPath inputStyle
  else
    text <| Item.topicLabel topic


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



-- MODEL


listSize : BoxId -> Model -> Size
listSize boxId model =
  case model.topicList |> Dict.get boxId of -- TODO: log error
    Just {size} -> size
    Nothing -> Size 0 0
