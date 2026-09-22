module Sync exposing (doc)

import ModelBase exposing (..)

import Crdt as C
import Crdt.Id

import Dict exposing (Dict)


-- the "Document"

type alias Model =
  { topics : Dict Id Topic
  }


-- the "Schema"

type alias ModelDoc =
  { topics : C.Ref Model (C.DictK C.Nested Topic) (Dict Id Topic)
  , schema : C.Schema C.Nested Model
  }


type alias TopicDoc =
  { id : C.Ref Topic (C.Variants TopicId) TopicId
  , icon : C.Ref Topic (C.Variants (Maybe Icon)) (Maybe Icon)
  , text : C.Ref Topic C.Settable String
  , size : C.Ref Topic C.Nested TextSize
  , assocIds : C.Ref Topic (C.ListK C.Fixed (C.Variants AssocId) AssocId) (List AssocId)
  , schema : C.Schema C.Nested Topic
  }


type alias TopicIdDoc =
  { topicId : C.Ref TopicId C.Settable Id
  , schema : C.Schema (C.Variants TopicId) TopicId
  }


type alias AssocIdDoc =
  { assocId : C.Ref AssocId C.Settable Id
  , schema : C.Schema (C.Variants AssocId) AssocId
  }


type alias IconDoc =
  { just : C.Ref (Maybe Icon) C.Settable Icon
  , schema : C.Schema (C.Variants (Maybe Icon)) (Maybe Icon)
  }


type alias TextSizeDoc =
  { view : C.Ref TextSize C.Nested Size
  , editor : C.Ref TextSize C.Nested Size
  , schema : C.Schema C.Nested TextSize
  }


type alias SizeDoc =
  { w : C.Ref Size C.Settable Int
  , h : C.Ref Size C.Settable Int
  , schema : C.Schema C.Nested Size
  }


modelDoc : ModelDoc
modelDoc =
  C.record Model ModelDoc
    |> C.field "topics" .topics (C.dict topicDoc)
    |> C.build


topicDoc : TopicDoc
topicDoc =
  C.record Topic TopicDoc
    |> C.field "id" .id topicIdDoc
    |> C.field "icon" .icon iconDoc
    |> C.field "text" .text C.text
    |> C.field "size" .size textSizeDoc
    |> C.field "assocIds" .assocIds (C.list assocIdDoc)
    |> C.build


topicIdDoc : TopicIdDoc
topicIdDoc =
  C.custom
    (\topicId value ->
      case value of
        TopicId id -> topicId id
    )
    TopicIdDoc
      |> C.variant1 "topicId" TopicId C.string
      |> C.buildCustom


assocIdDoc : AssocIdDoc
assocIdDoc =
  C.custom
    (\assocId value ->
      case value of
        AssocId id -> assocId id
    )
    AssocIdDoc
      |> C.variant1 "assocId" AssocId C.string
      |> C.buildCustom


iconDoc : IconDoc
iconDoc =
  C.custom
    (\just nothing value ->
      case value of
        Just val ->
          just val
        Nothing ->
          nothing
    )
    IconDoc
      |> C.variant1 "just" Just C.string
      |> C.variant0 "nothing" Nothing
      |> C.buildCustom


textSizeDoc : TextSizeDoc
textSizeDoc =
  C.record TextSize TextSizeDoc
    |> C.field "view" .view sizeDoc
    |> C.field "editor" .editor sizeDoc
    |> C.build


sizeDoc : SizeDoc
sizeDoc =
  C.record Size SizeDoc
    |> C.field "w" .w C.int
    |> C.field "h" .h C.int
    |> C.build


-- the live document

doc =
    C.init (Crdt.Id.replica "test") modelDoc.schema
