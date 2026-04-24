module TopicMap.TopicMapDef exposing (Model, TopicMap, MapTopic, Msg(..), init, encode, decoder)

import ModelBase exposing (..)
import Dict exposing (Dict)
import Json.Decode as D
import Json.Encode as E



type alias Model =
  Dict BoxId TopicMap


init : Model
init =
  Dict.singleton rootBoxId <| TopicMap rootBoxId (Rectangle 0 0 0 0) (Point 0 0) Dict.empty


type alias TopicMap =
  { id : BoxId
  , rect : Rectangle
  , scroll : Point
  , topics : Dict Id MapTopic
  }


type alias MapTopic =
  { id : Id
  , pos : Point
  , expansion : Expansion -- "effective expansion", computed/transient,
                          -- used for both limbo-rendering and auto-sizing
  }


type Msg
  = AddTopic Id BoxId Point -- random pos



-- JSON


-- Encode

encode : Model -> E.Value
encode model =
  model
    |> Dict.values
    |> E.list encodeTopicMap


encodeTopicMap : TopicMap -> E.Value
encodeTopicMap map =
  E.object
    [ ("id", E.int map.id)
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
    [ ("id", E.int topic.id)
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
  topicMapDecoder |> toDictDecoder


topicMapDecoder : D.Decoder TopicMap
topicMapDecoder =
  D.map4 TopicMap
    (D.field "id" D.int)
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
    (D.field "topics" (mapTopicDecoder |> toDictDecoder))


mapTopicDecoder : D.Decoder MapTopic
mapTopicDecoder =
  D.map3 MapTopic
    (D.field "id" D.int)
    (D.field "pos" <| D.map2
      Point
        (D.field "x" D.int)
        (D.field "y" D.int)
    )
    (D.succeed Collapsed) -- "effective expansion" is computed/transient, just dummy value here
