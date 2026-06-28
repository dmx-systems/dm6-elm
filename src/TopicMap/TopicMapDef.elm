module TopicMap.TopicMapDef exposing (Model, TopicMap, MapTopic, DragState(..), DragMode(..),
  Msg(..), init, encode, decoder)

import ModelBase exposing (..)

import Dict exposing (Dict)
import Json.Decode as D
import Json.Encode as E
import Time



type alias Model =
  { maps : Dict Id TopicMap
  -- State machine to initiate a drag from a TopicMap box.
  -- Available while a drag that started from a TopicMap box is active.
  , dragState : Maybe DragState -- transient
  }


init : Model
init =
  { maps =
      Dict.singleton
        (toBoxId rootBoxId)
        (TopicMap rootBoxId (Rectangle 0 0 0 0) (Point 0 0) Dict.empty)
  -- transient
  , dragState = Nothing
  }


type alias TopicMap =
  { id : BoxId
  , rect : Rectangle
  , scroll : Point
  , topics : Dict Id MapTopic
  }


type alias MapTopic =
  { id : TopicId
  , pos : Point
  , expansion : Expansion -- "effective expansion", computed/transient,
                          -- used for both limbo-rendering and auto-sizing
  }


type DragState
  = WaitForStartTime
  | DragEngaged Time.Posix    -- start time, entered once start time arrives
  | WaitForEndTime Time.Posix -- start time (buffered), entered on 1st move (once engaged)
  | Drag DragMode             -- entered once end time arrives


type DragMode
  = DragTopic Point -- original topic position
  | DraftAssoc


type Msg
  = AssocClicked AssocId BoxPath
  | GotTime Time.Posix
  | GotRandomPos TopicId BoxId Point



-- JSON


-- Encode

encode : Model -> E.Value
encode model =
  E.list encodeBoxProps (model.maps |> Dict.values)


encodeBoxProps : TopicMap -> E.Value
encodeBoxProps topicMap =
  E.object
    [ ("id", encodeBoxId topicMap.id)
    , ("rect", E.object
        [ ("x1", E.int topicMap.rect.x1)
        , ("y1", E.int topicMap.rect.y1)
        , ("x2", E.int topicMap.rect.x2)
        , ("y2", E.int topicMap.rect.y2)
        ]
      )
    , ("scroll", E.object
        [ ("x", E.int topicMap.scroll.x)
        , ("y", E.int topicMap.scroll.y)
        ]
      )
    , ("topics", topicMap.topics
        |> Dict.values
        |> E.list encodeTopicProps)
    ]


encodeTopicProps : MapTopic -> E.Value
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
    (D.succeed Nothing)


boxPropsDecoder : D.Decoder TopicMap
boxPropsDecoder =
  D.map4 TopicMap
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
    (D.field "topics" (toDictDecoderWith toTopicId topicPropsDecoder))


topicPropsDecoder : D.Decoder MapTopic
topicPropsDecoder =
  D.map3 MapTopic
    (D.field "id" topicIdDecoder)
    (D.field "pos" <| D.map2
      Point
        (D.field "x" D.int)
        (D.field "y" D.int)
    )
    (D.succeed Collapsed) -- "effective expansion" is computed/transient, just dummy value here
