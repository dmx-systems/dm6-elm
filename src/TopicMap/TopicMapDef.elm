module TopicMap.TopicMapDef exposing (Model, TopicMap, MapTopic, DragState(..), DragMode(..),
  Msg(..), init, encode, decoder)

import ModelBase exposing (..)

import Dict exposing (Dict)
import Json.Decode as D
import Json.Encode as E
import Time



type alias Model =
  { topicMaps : Dict Id TopicMap
  , dragState : DragState
  , hover : Maybe Target -- TODO: make it explicitly TopicTarget?
  }


init : Model
init =
  { topicMaps =
      Dict.singleton
        (toBoxId rootBoxId)
        (TopicMap rootBoxId (Rectangle 0 0 0 0) (Point 0 0) Dict.empty)
  -- transient
  , dragState = NoDrag
  , hover = Nothing
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
  = WaitForStartTime TopicId BoxPath Point -- start point (mouse)
  | DragEngaged Time.Posix TopicId BoxPath Point -- start point (mouse)
  | WaitForEndTime Time.Posix TopicId BoxPath Point -- start point (mouse)
  | Drag DragMode TopicId BoxPath Point Point (Maybe Target) -- orig topic pos
                                                        -- last point (mouse)
  | NoDrag


type DragMode
  = DragTopic
  | DraftAssoc


type Msg
  -- Mouse
  = DownOnTopic TopicId BoxPath (Point, PointerType) -- mouse down on topic, drag engaged
  | Time Time.Posix
  -- TopicMap
  | GotRandomPos TopicId BoxId Point



-- JSON


-- Encode

encode : Model -> E.Value
encode model =
  E.object
    [ ("topicMaps", E.list encodeTopicMap (model.topicMaps |> Dict.values))
    ]


encodeTopicMap : TopicMap -> E.Value
encodeTopicMap map =
  E.object
    [ ("id", encodeBoxId map.id)
    , ("rect", E.object
        [ ("x1", E.int map.rect.x1)
        , ("y1", E.int map.rect.y1)
        , ("x2", E.int map.rect.x2)
        , ("y2", E.int map.rect.y2)
        ]
      )
    , ("scroll", E.object
        [ ("x", E.int map.scroll.x)
        , ("y", E.int map.scroll.y)
        ]
      )
    , ("topics", map.topics
        |> Dict.values
        |> E.list encodeMapTopic)
    ]


encodeMapTopic : MapTopic -> E.Value
encodeMapTopic topic =
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
  D.map3 Model
    (D.field "topicMaps" (toDictDecoderWith toBoxId topicMapDecoder))
    (D.succeed NoDrag)
    (D.succeed Nothing)


topicMapDecoder : D.Decoder TopicMap
topicMapDecoder =
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
    (D.field "topics" (toDictDecoderWith toTopicId mapTopicDecoder))


mapTopicDecoder : D.Decoder MapTopic
mapTopicDecoder =
  D.map3 MapTopic
    (D.field "id" topicIdDecoder)
    (D.field "pos" <| D.map2
      Point
        (D.field "x" D.int)
        (D.field "y" D.int)
    )
    (D.succeed Collapsed) -- "effective expansion" is computed/transient, just dummy value here
