module Feature.CrdtDef exposing (Model, init, refs, topicTextRef, topicViewSizeRef,
  topicEditorSizeRef)

import ModelBase exposing (..)

import Crdt as C
import Crdt.Doc as Doc exposing (Doc, Version)
import Crdt.Id

import Dict exposing (Dict)



type alias Model =
  { doc : Doc DocModel
  , lastStored : Version
  , editStartVersion : Maybe Version
  }


init : Model
init =
  let
    doc = C.init (Crdt.Id.replica "my-replica") modelDoc.schema -- TODO: replica ID
  in
  { doc = doc
  , lastStored = Doc.version doc
  , editStartVersion = Nothing
  }



-- SCHEMA


type alias ModelDoc =
  { topics : C.Ref DocModel (C.DictK C.Nested Topic) (Dict Id Topic)
  , schema : C.Schema C.Nested DocModel
  }


modelDoc : ModelDoc
modelDoc =
  C.record DocModel ModelDoc
    |> C.field "topics" .topics topicsDict
    |> C.build


topicsDict :
  C.Crdt
    (C.DictK C.Nested Topic)
    (Dict Id Topic)
    { key :
        Id
          -> C.Ref DocModel (C.DictK C.Nested Topic) (Dict Id Topic)
          -> C.Ref DocModel C.Nested Topic
    }
topicsDict =
  C.dict topicDoc


topicRef : Id -> C.Ref DocModel C.Nested Topic
topicRef id =
  refs.topics |> topicsDict.key id


refs : ModelDoc
refs =
  modelDoc


-- Topic

type alias TopicDoc =
  { id : C.Ref Topic (C.Variants TopicId) TopicId
  , icon : C.Ref Topic (C.Variants (Maybe Icon)) (Maybe Icon)
  , text : C.Ref Topic C.Settable String
  , size : C.Ref Topic C.Nested TextSize
  , assocIds : C.Ref Topic (C.ListK C.Fixed (C.Variants AssocId) AssocId) (List AssocId)
  , schema : C.Schema C.Nested Topic
  }


topicDoc : TopicDoc
topicDoc =
  C.record Topic TopicDoc
    |> C.field "id" .id topicIdDoc
    |> C.field "icon" .icon iconDoc
    |> C.field "text" .text C.text
    |> C.field "size" .size textSizeDoc
    |> C.field "assocIds" .assocIds (C.list assocIdDoc)
    |> C.build


topicTextRef : Id -> C.Ref DocModel C.Settable String
topicTextRef id =
  topicRef id |> C.at topicDoc.text


topicSizeRef : Id -> C.Ref DocModel C.Nested TextSize
topicSizeRef id =
  topicRef id |> C.at topicDoc.size


topicViewSizeRef : Id -> C.Ref DocModel C.Nested Size
topicViewSizeRef id =
  topicSizeRef id |> C.at textSizeDoc.view


topicEditorSizeRef : Id -> C.Ref DocModel C.Nested Size
topicEditorSizeRef id =
  topicSizeRef id |> C.at textSizeDoc.editor


type alias TextSizeDoc =
  { view : C.Ref TextSize C.Nested Size
  , editor : C.Ref TextSize C.Nested Size
  , schema : C.Schema C.Nested TextSize
  }


textSizeDoc : TextSizeDoc
textSizeDoc =
  C.record TextSize TextSizeDoc
    |> C.field "view" .view sizeDoc
    |> C.field "editor" .editor sizeDoc
    |> C.build


type alias SizeDoc =
  { w : C.Ref Size C.Settable Int
  , h : C.Ref Size C.Settable Int
  , schema : C.Schema C.Nested Size
  }


sizeDoc : SizeDoc
sizeDoc =
  C.record Size SizeDoc
    |> C.field "w" .w C.int
    |> C.field "h" .h C.int
    |> C.build


type alias TopicIdDoc =
  { topicId : C.Ref TopicId C.Settable Id
  , schema : C.Schema (C.Variants TopicId) TopicId
  }


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


type alias AssocIdDoc =
  { assocId : C.Ref AssocId C.Settable Id
  , schema : C.Schema (C.Variants AssocId) AssocId
  }


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


type alias IconDoc =
  { just : C.Ref (Maybe Icon) C.Settable Icon
  , schema : C.Schema (C.Variants (Maybe Icon)) (Maybe Icon)
  }


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
