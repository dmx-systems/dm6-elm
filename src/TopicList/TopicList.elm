module TopicList.TopicList exposing (..)

import Box
import Config as C
import ExtensionDef exposing (ExtManager)
import Item
import Model exposing (Model, Msg)
import ModelBase exposing (..)

import Html exposing (Html, ul, li, text)
import Html.Attributes exposing (style)
import String exposing (fromInt)



-- VIEW


-- For the fullscreen box boxPath is empty
view : BoxId -> BoxPath -> ExtManager -> Model -> Html Msg
view boxId boxPath ext model =
  ul
    (boxStyle boxId model)
    (viewBox boxId model)


viewBox : BoxId -> Model -> List (Html Msg)
viewBox boxId model =
  case Box.topics boxId model of
    Just topics ->
      topics |> List.map
        (\topic ->
          li [] [ text <| Item.topicLabel topic ]
        )
    Nothing -> [ text "ItemSet ?" ]


boxStyle : BoxId -> Model -> Attrs Msg
boxStyle boxId model =
  let
    width = 240 -- TODO
    height = 100 -- TODO
    r = fromInt C.whiteBoxRadius ++ "px"
  in
  if Box.isFullscreen boxId model then
    []
  else
    [ style "position" "absolute"
    , style "left" <| fromInt -C.topicBorderWidth ++ "px"
    , style "top" <| fromInt (C.topicHeight - 2 * C.topicBorderWidth - 16) ++ "px" -- 16?
    , style "width" <| fromInt width ++ "px"
    , style "height" <| fromInt height ++ "px"
    , style "border-radius" <| "0 " ++ r ++ " " ++ r ++ " " ++ r
    , style "padding-top" "14px"
    , style "background-color" "white"
    ]
    ++ boxBorderStyle


boxBorderStyle : Attrs Msg
boxBorderStyle =
  [ style "border-width" <| fromInt C.topicBorderWidth ++ "px"
  , style "border-style" "solid"
  , style "box-sizing" "border-box"
  ]
