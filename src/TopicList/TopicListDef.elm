module TopicList.TopicListDef exposing (Model, TopicList, init, encode, decoder)

import ModelBase exposing (..)
import Dict exposing (Dict)
import Json.Decode as D
import Json.Encode as E



type alias Model =
  Dict Id TopicList


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
    [ ("id", encodeBoxId list.id)
    , ("size", E.object
        [ ("w", E.int list.size.w)
        , ("h", E.int list.size.h)
        ]
      )
    ]


-- Decode

decoder : D.Decoder Model
decoder =
  toDictDecoderWith toBoxId topicListDecoder


topicListDecoder : D.Decoder TopicList
topicListDecoder =
  D.map2 TopicList
    (D.field "id" boxIdDecoder)
    (D.field "size" <| D.map2 Size
      (D.field "w" D.int)
      (D.field "h" D.int)
    )
