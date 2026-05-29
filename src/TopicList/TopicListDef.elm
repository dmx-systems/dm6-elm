module TopicList.TopicListDef exposing (Model, BoxProps, DragState, DropTarget(..), init,
  resetTransient, encode, decoder)

import ModelBase exposing (..)
import Dict exposing (Dict)
import Json.Decode as D
import Json.Encode as E



type alias Model =
  { boxProps : Dict Id BoxProps
  , dragState : Maybe DragState -- only available while DragStarted (Mouse state), transient
  }


init : Model
init =
  { boxProps = Dict.empty
  , dragState = Nothing
  }


resetTransient : Model -> Model
resetTransient model =
  { model | dragState = Nothing }


type alias BoxProps =
  { id : BoxId
  , order : List TopicId
  , size : Size
  }


type alias DragState =
  { elemPos : Point -- position of dragged element
  , lastPos : Point -- last pointer position
  , dropTarget : Maybe DropTarget
  }


type DropTarget
  = Drop Target
  | InsertBefore Target



-- JSON


-- Encode

encode : Model -> E.Value
encode model =
  E.list encodeTopicList (model.boxProps |> Dict.values)


encodeTopicList : BoxProps -> E.Value
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
    (toDictDecoderWith toBoxId topicListDecoder)
    (D.succeed Nothing)


topicListDecoder : D.Decoder BoxProps
topicListDecoder =
  D.map3 BoxProps
    (D.field "id" boxIdDecoder)
    (D.field "order" (D.list topicIdDecoder))
    (D.field "size" <| D.map2 Size
      (D.field "w" D.int)
      (D.field "h" D.int)
    )
