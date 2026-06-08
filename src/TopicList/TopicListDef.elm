module TopicList.TopicListDef exposing (Model, BoxProps, DragState, DropTarget(..), Targets,
  init, resetTransient, encode, decoder)

import ModelBase exposing (..)
import Dict exposing (Dict)
import Json.Decode as D
import Json.Encode as E

import Array exposing (Array)



type alias Model =
  { boxProps : Dict Id BoxProps
  , dragState : DragState -- transient
  }


init : Model
init =
  { boxProps = Dict.empty
  , dragState = DragState Nothing Nothing
  }


resetTransient : Model -> Model
resetTransient model =
  { model | dragState = DragState Nothing Nothing }


type alias BoxProps =
  { id : BoxId
  , order : List TopicId
  , size : Size
  }


type alias DragState =
  -- position of dragging list item
  -- available while a drag that started from a list rendering is active
  { itemPos : Maybe Point
  -- accepted drop location within a list rendering
  -- Note: the drag has not necessarily started from a list rendering (but another renderer)
  , dropTarget : Maybe DropTarget
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
  D.map2 Model
    (toDictDecoderWith toBoxId topicListDecoder)
    (D.succeed <| DragState Nothing Nothing)


topicListDecoder : D.Decoder BoxProps
topicListDecoder =
  D.map3 BoxProps
    (D.field "id" boxIdDecoder)
    (D.field "order" (D.list topicIdDecoder))
    (D.field "size" <| D.map2 Size
      (D.field "w" D.int)
      (D.field "h" D.int)
    )
