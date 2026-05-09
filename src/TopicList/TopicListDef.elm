module TopicList.TopicListDef exposing (Model, ViewProps, init, encode, decoder)

import ModelBase exposing (..)
import Dict exposing (Dict)
import Json.Decode as D
import Json.Encode as E



type alias Model =
  { viewProps : Dict Id ViewProps
  , dragState : DragState -- transient
  }


init : Model
init =
  { viewProps = Dict.empty
  , dragState = NoDrag
  }


type alias ViewProps =
  { id : BoxId
  , order : List TopicId
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
    [ ("viewProps", E.list encodeTopicList (model.viewProps |> Dict.values))
    ]


encodeTopicList : ViewProps -> E.Value
encodeTopicList list =
  E.object
    [ ("id", encodeBoxId list.id)
    , ("order", E.list encodeTopicId list.order)
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
    (D.field "viewProps" (toDictDecoderWith toBoxId topicListDecoder))
    (D.succeed NoDrag)


topicListDecoder : D.Decoder ViewProps
topicListDecoder =
  D.map3 ViewProps
    (D.field "id" boxIdDecoder)
    (D.field "order" (D.list topicIdDecoder))
    (D.field "size" <| D.map2 Size
      (D.field "w" D.int)
      (D.field "h" D.int)
    )
