module TopicList.TopicList exposing (view, listSize)

import Box
import Config as C
import Dict
import Env exposing (ExtManager)
import Item
import Model exposing (Model, Msg)
import ModelBase exposing (..)

import Html exposing (Html, div, ul, li, text)
import Html.Attributes exposing (style)
import String exposing (fromInt, fromFloat)



-- VIEW


-- For the fullscreen box boxPath is empty
view : BoxId -> BoxPath -> ExtManager -> Model -> Html Msg
view boxId boxPath ext model =
  div
    ( listStyle boxId model )
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


listStyle : BoxId -> Model -> Attrs Msg
listStyle boxId model =
  let
    size = listSize boxId model
    r = fromInt C.whiteBoxRadius ++ "px"
  in
  if Box.isFullscreen boxId model then
    []
  else
    [ style "position" "absolute"
    , style "left" <| fromInt -C.topicBorderWidth ++ "px"
    , style "top" <| fromInt (C.topicHeight - 2 * C.topicBorderWidth) ++ "px"
    , style "width" <| fromInt size.w ++ "px"
    , style "height" <| fromInt size.h ++ "px"
    , style "border-radius" <| "0 " ++ r ++ " " ++ r ++ " " ++ r
    , style "background-color" "white"
    ]
    ++ listFontStyle
    ++ listBorderStyle


listFontStyle : Attrs Msg
listFontStyle =
  [ style "font-family" C.mainFont
  , style "font-size" <| fromInt C.contentFontSize ++ "px"
  , style "line-height" <| fromFloat C.topicLineHeight
  ]


listBorderStyle : Attrs Msg
listBorderStyle =
  [ style "border-width" <| fromInt C.topicBorderWidth ++ "px"
  , style "border-style" "solid"
  , style "box-sizing" "border-box"
  ]



-- MODEL


listSize : BoxId -> Model -> Size
listSize boxId model =
  case model.topicList |> Dict.get boxId of
    Just {size} -> size
    Nothing -> Size 0 0
