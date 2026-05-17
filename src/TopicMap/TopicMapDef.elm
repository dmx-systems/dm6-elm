module TopicMap.TopicMapDef exposing (Model, BoxProps, TopicProps, DragState(..), DragMode(..),
  Msg(..), init, encode, decoder)

import ModelBase exposing (..)

import Dict exposing (Dict)
import Json.Decode as D
import Json.Encode as E
import Time



type alias Model =
  { boxProps : Dict Id BoxProps
  , dragState : DragState
  }


init : Model
init =
  { boxProps =
      Dict.singleton
        (toBoxId rootBoxId)
        (BoxProps rootBoxId (Rectangle 0 0 0 0) (Point 0 0) Dict.empty)
  -- transient
  , dragState = NoDrag
  }


type alias BoxProps =
  { id : BoxId
  , rect : Rectangle
  , scroll : Point
  , topicProps : Dict Id TopicProps
  }


type alias TopicProps =
  { id : TopicId
  , pos : Point
  , expansion : Expansion -- "effective expansion", computed/transient,
                          -- used for both limbo-rendering and auto-sizing
  }


type DragState
  = WaitForStartTime
  | DragEngaged Time.Posix    -- start time
  | WaitForEndTime Time.Posix -- start time (buffered)
  | Drag DragMode Point Point (Maybe Target) -- original topic position
                                             -- last pointer position
                                             -- accepted drop target
  | NoDrag


type DragMode
  = DragTopic
  | DraftAssoc


type Msg
  = Time Time.Posix
  | GotRandomPos TopicId BoxId Point



-- JSON


-- Encode

encode : Model -> E.Value
encode model =
  E.list encodeBoxProps (model.boxProps |> Dict.values)


encodeBoxProps : BoxProps -> E.Value
encodeBoxProps boxProps =
  E.object
    [ ("id", encodeBoxId boxProps.id)
    , ("rect", E.object
        [ ("x1", E.int boxProps.rect.x1)
        , ("y1", E.int boxProps.rect.y1)
        , ("x2", E.int boxProps.rect.x2)
        , ("y2", E.int boxProps.rect.y2)
        ]
      )
    , ("scroll", E.object
        [ ("x", E.int boxProps.scroll.x)
        , ("y", E.int boxProps.scroll.y)
        ]
      )
    , ("topicProps", boxProps.topicProps
        |> Dict.values
        |> E.list encodeTopicProps)
    ]


encodeTopicProps : TopicProps -> E.Value
encodeTopicProps topic =
  E.object
    [ ("id", E.int (toTopicId topic.id))
    , ("pos", E.object
        [ ("x", E.int topic.pos.x)
        , ("y", E.int topic.pos.y)
        ]
      )
    -- Note: "effective expansion" is computed/transient, source of truth is in Box/BoxTopic
    ]


-- Decode

decoder : D.Decoder Model
decoder =
  D.map2 Model
    (toDictDecoderWith toBoxId boxPropsDecoder)
    (D.succeed NoDrag)


boxPropsDecoder : D.Decoder BoxProps
boxPropsDecoder =
  D.map4 BoxProps
    (D.field "id" boxIdDecoder)
    (D.field "rect" <| D.map4 Rectangle
      (D.field "x1" D.int)
      (D.field "y1" D.int)
      (D.field "x2" D.int)
      (D.field "y2" D.int)
    )
    (D.field "scroll" <| D.map2 Point
      (D.field "x" D.int)
      (D.field "y" D.int)
    )
    (D.field "topicProps" (toDictDecoderWith toTopicId topicPropsDecoder))


topicPropsDecoder : D.Decoder TopicProps
topicPropsDecoder =
  D.map3 TopicProps
    (D.field "id" topicIdDecoder)
    (D.field "pos" <| D.map2
      Point
        (D.field "x" D.int)
        (D.field "y" D.int)
    )
    (D.succeed Collapsed) -- "effective expansion" is computed/transient, just dummy value here
