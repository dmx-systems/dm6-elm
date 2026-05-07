module TopicList.TopicListDef exposing (Model, TopicList, init, encode, decoder)

import ModelBase exposing (..)
import Dict exposing (Dict)
import Json.Decode as D
import Json.Encode as E



type alias Model =
  { topicLists : Dict Id TopicList
  , dragState : DragState
  }


init : Model
init =
  { topicLists = Dict.empty
  , dragState = NoDrag
  }


type alias TopicList =
  { id : BoxId
  , size : Size
  }


type DragState
  = DragEngaged TopicId BoxPath
  | Drag TopicId BoxPath (Maybe Target)
  | NoDrag



-- JSON


-- Encode

encode : Model -> E.Value
encode model =
  E.object
    [ ("topicLists", E.list encodeTopicList (model.topicLists |> Dict.values))
    ]


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
  D.map2 Model
    (D.field "topicLists" (toDictDecoderWith toBoxId topicListDecoder))
    (D.succeed NoDrag)


topicListDecoder : D.Decoder TopicList
topicListDecoder =
  D.map2 TopicList
    (D.field "id" boxIdDecoder)
    (D.field "size" <| D.map2 Size
      (D.field "w" D.int)
      (D.field "h" D.int)
    )
