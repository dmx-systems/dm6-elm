module TopicList.TopicList exposing (view, listSize)

import Box
import Config as C
import Dict
import Env exposing (ExtManager)
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
    [ viewList boxId model ]


viewList : BoxId -> Model -> Html Msg
viewList boxId model =
  ul
    []
    ( Box.topics boxId model
        |> List.map
          (\topic ->
            li []
              ( [ text <| Item.topicLabel topic ]
                ++
                if Item.isBox topic.id model then
                  [ viewList topic.id model ]
                else
                  []
              )
          )
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
  , style "line-height" <| fromFloat C.topicLineHeight
  ]



-- MODEL


listSize : BoxId -> Model -> Size
listSize boxId model =
  case model.topicList |> Dict.get boxId of
    Just {size} -> size
    Nothing -> Size 0 0
