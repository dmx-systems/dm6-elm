module TopicList.TopicListDef exposing (Model, init, encode, decoder)

import ModelBase exposing (..)
import Dict exposing (Dict)
import Json.Decode as D
import Json.Encode as E



type alias Model =
  Dict BoxId TopicList


init : Model
init =
  Dict.empty


type alias TopicList =
  { id : BoxId
  , size : Size
  }



-- JSON


-- Encode

encode : Model -> E.Value
encode model =
  model
    |> Dict.values
    |> E.list encodeTopicList


encodeTopicList : TopicList -> E.Value
encodeTopicList list =
  E.object
    [ ("id", E.int list.id)
    , ("size", E.object
        [ ("w", E.int list.size.w)
        , ("h", E.int list.size.h)
        ]
      )
    ]


-- Decode

decoder : D.Decoder Model
decoder =
  topicListDecoder |> toDictDecoder


topicListDecoder : D.Decoder TopicList
topicListDecoder =
  D.map2 TopicList
    (D.field "id" D.int)
    (D.field "size" <| D.map2 Size
      (D.field "w" D.int)
      (D.field "h" D.int)
    )
