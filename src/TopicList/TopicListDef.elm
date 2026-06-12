module TopicList.TopicListDef exposing (Model, BoxProps, DropTarget(..), Targets, init,
  resetTransient, encode, decoder)

import ModelBase exposing (..)
import Dict exposing (Dict)
import Json.Decode as D
import Json.Encode as E

import Array exposing (Array)



type alias Model =
  { boxProps : Dict Id BoxProps
  -- position of dragging list item -- transient
  -- available while a drag that started from a list rendering is active
  , dragPos : Maybe Point
  -- accepted drop location within a list rendering -- transient
  -- Note: the drag has not necessarily started from a list rendering (but another renderer)
  , dropTarget : Maybe DropTarget
  }


init : Model
init =
  { boxProps = Dict.empty
  , dragPos = Nothing
  , dropTarget = Nothing
  }


resetTransient : Model -> Model
resetTransient model =
  { model
  | dragPos = Nothing
  , dropTarget = Nothing
  }


type alias BoxProps =
  { id : BoxId
  , order : List TopicId
  , size : Size
  }


type DropTarget
  = Drop Target
  | InsertBefore Target


type alias Targets = Array (Level, Target)



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
  D.map3 Model
    (toDictDecoderWith toBoxId topicListDecoder)
    (D.succeed Nothing)
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
