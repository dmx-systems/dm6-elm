module Style exposing (..)

import Model exposing (..)
import Html exposing (Attribute)
import Html.Attributes exposing (style)



-- STYLE


topicStyle : Point -> List (Attribute Msg)
topicStyle pos =
  [ style "position" "absolute"
  , style "left" <| String.fromInt pos.x ++ "px"
  , style "top" <| String.fromInt pos.y ++ "px"
  , style "width" "30px"
  , style "height" "30px"
  , style "border-radius" "15px"
  , style "background-color" "green"
  ]
