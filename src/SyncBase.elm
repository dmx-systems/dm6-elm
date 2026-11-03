module SyncBase exposing (topicIdDoc, boxIdDoc, sizeDoc, pointDoc, rectDoc, expansionDoc)

import ModelBase exposing (..)

import Crdt as C



type alias TopicIdDoc =
  { topicId : C.Ref TopicId C.Settable Id
  , schema : C.Schema (C.Variants TopicId) TopicId
  }


type alias BoxIdDoc =
  { boxId : C.Ref BoxId (C.Variants TopicId) TopicId
  , schema : C.Schema (C.Variants BoxId) BoxId
  }


type alias SizeDoc =
  { w : C.Ref Size C.Settable Int
  , h : C.Ref Size C.Settable Int
  , schema : C.Schema C.Nested Size
  }


type alias PointDoc =
  { x : C.Ref Point C.Settable Int
  , y : C.Ref Point C.Settable Int
  , schema : C.Schema C.Nested Point
  }


type alias RectangleDoc =
  { x1 : C.Ref Rectangle C.Settable Int
  , y1 : C.Ref Rectangle C.Settable Int
  , x2 : C.Ref Rectangle C.Settable Int
  , y2 : C.Ref Rectangle C.Settable Int
  , schema : C.Schema C.Nested Rectangle
  }


type alias ExpansionDoc =
  { schema : C.Schema (C.Variants Expansion) Expansion
  }


--

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


boxIdDoc : BoxIdDoc
boxIdDoc =
  C.custom
    (\boxId value ->
      case value of
        BoxId topicId -> boxId topicId
    )
    BoxIdDoc
      |> C.variant1 "boxId" BoxId topicIdDoc
      |> C.buildCustom


sizeDoc : SizeDoc
sizeDoc =
  C.record Size SizeDoc
    |> C.field "w" .w C.int
    |> C.field "h" .h C.int
    |> C.build


pointDoc : PointDoc
pointDoc =
  C.record Point PointDoc
    |> C.field "x" .x C.int
    |> C.field "y" .y C.int
    |> C.build


rectDoc : RectangleDoc
rectDoc =
  C.record Rectangle RectangleDoc
    |> C.field "x1" .x1 C.int
    |> C.field "y1" .y1 C.int
    |> C.field "x2" .x2 C.int
    |> C.field "y2" .y2 C.int
    |> C.build


expansionDoc : ExpansionDoc
expansionDoc =
  C.custom
    (\collapsed expanded value ->
      case value of
        Collapsed ->
          collapsed
        Expanded ->
          expanded
    )
    ExpansionDoc
      |> C.variant0 "collapsed" Collapsed
      |> C.variant0 "expanded" Expanded
      |> C.buildCustom
