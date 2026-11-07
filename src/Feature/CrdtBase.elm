module Feature.CrdtBase exposing (topicDoc, topicIdDoc, textSizeDoc, assocDoc,
  boxDoc, boxIdDoc, boxTopicsDict, boxTopicDoc, itemSetDoc,
  sizeDoc, pointDoc, rectDoc, expansionDoc)

import ModelBase exposing (..)
import Renderer exposing (Renderer)

import Crdt as C

import Dict exposing (Dict)



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


type alias IconDoc =
  { just : C.Ref (Maybe Icon) C.Settable Icon -- only variants with payload needs to be modelled
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


-- Assoc

type alias AssocDoc =
  { id : C.Ref Assoc (C.Variants AssocId) AssocId
  , assocType : C.Ref Assoc (C.Variants AssocType) AssocType
  , topicId1 : C.Ref Assoc (C.Variants TopicId) TopicId
  , topicId2 : C.Ref Assoc (C.Variants TopicId) TopicId
  , schema : C.Schema C.Nested Assoc
  }


assocDoc : AssocDoc
assocDoc =
  C.record Assoc AssocDoc
    |> C.field "id" .id assocIdDoc
    |> C.field "assocType" .assocType assocTypeDoc
    |> C.field "topicId1" .topicId1 topicIdDoc
    |> C.field "topicId2" .topicId2 topicIdDoc
    |> C.build


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


type alias AssocTypeDoc =
  { schema : C.Schema (C.Variants AssocType) AssocType
  }


assocTypeDoc : AssocTypeDoc
assocTypeDoc =
  C.custom
    (\association hierarchy value ->
      case value of
        Association -> association
        Hierarchy -> hierarchy
    )
    AssocTypeDoc
      |> C.variant0 "association" Association
      |> C.variant0 "hierarchy" Hierarchy
      |> C.buildCustom


-- Box

type alias BoxDoc =
  { id : C.Ref Box (C.Variants BoxId) BoxId
  , itemSetId : C.Ref Box C.Settable Id
  , topics : C.Ref Box (C.DictK C.Nested BoxTopic) (Dict Id BoxTopic)
  , renderer : C.Ref Box (C.Variants Renderer) Renderer
  , schema : C.Schema C.Nested Box
  }


boxDoc : BoxDoc
boxDoc =
  C.record Box BoxDoc
    |> C.field "id" .id boxIdDoc
    |> C.field "itemSetId" .itemSetId C.string
    |> C.field "topics" .topics boxTopicsDict
    |> C.field "renderer" .renderer rendererDoc
    |> C.build


type alias BoxIdDoc =
  { boxId : C.Ref BoxId (C.Variants TopicId) TopicId
  , schema : C.Schema (C.Variants BoxId) BoxId
  }


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


-- Can't add type annotation here. Importing SyncModel would create cyclic dependency.
-- Note: although boxTopicsDict is not top-level but nested inside Box schema its type
-- annotation still refers to top-level SyncModel type.
--
-- boxTopicsDict :
--   C.Crdt
--     (C.DictK C.Nested BoxTopic) (Dict Id BoxTopic)
--     { key :
--         Id
--           -> C.Ref SyncModel (C.DictK C.Nested BoxTopic) (Dict Id BoxTopic)
--           -> C.Ref SyncModel C.Nested BoxTopic
--     }
boxTopicsDict =
  C.dict boxTopicDoc


type alias BoxTopicDoc =
  { id : C.Ref BoxTopic (C.Variants TopicId) TopicId
  , expansion : C.Ref BoxTopic (C.Variants Expansion) Expansion
  , schema : C.Schema C.Nested BoxTopic
  }


boxTopicDoc : BoxTopicDoc
boxTopicDoc =
  C.record BoxTopic BoxTopicDoc
    |> C.field "id" .id topicIdDoc
    |> C.field "expansion" .expansion expansionDoc
    |> C.build


type alias RendererDoc =
  { schema : C.Schema (C.Variants Renderer) Renderer
  }


rendererDoc : RendererDoc
rendererDoc =
  C.custom
    (\topicMap topicList value ->
      case value of
        Renderer.TopicMap ->
          topicMap
        Renderer.TopicList ->
          topicList
    )
    RendererDoc
      |> C.variant0 "topicMap" Renderer.TopicMap
      |> C.variant0 "topicList" Renderer.TopicList
      |> C.buildCustom


-- Item Set

type alias ItemSetDoc =
  { id : C.Ref ItemSet C.Settable Id
  , items : C.Ref ItemSet (C.ListK C.Fixed C.Nested SetItem) (List SetItem)
  , schema : C.Schema C.Nested ItemSet
  }


itemSetDoc : ItemSetDoc
itemSetDoc =
  C.record ItemSet ItemSetDoc
    |> C.field "id" .id C.string
    |> C.field "items" .items (C.list setItemDoc)
    |> C.build


type alias SetItemDoc =
  { id : C.Ref SetItem (C.Variants ItemId) ItemId
  , schema : C.Schema C.Nested SetItem
  }


setItemDoc : SetItemDoc
setItemDoc =
  C.record SetItem SetItemDoc
    |> C.field "id" .id itemIdDoc
    |> C.build


type alias ItemIdDoc =
  { t : C.Ref ItemId (C.Variants TopicId) TopicId
  , a : C.Ref ItemId (C.Variants AssocId) AssocId
  , schema : C.Schema (C.Variants ItemId) ItemId
  }


itemIdDoc : ItemIdDoc
itemIdDoc =
  C.custom
    (\t a value ->
      case value of
        T topicId -> t topicId
        A assocId -> a assocId
    )
    ItemIdDoc
      |> C.variant1 "t" T topicIdDoc
      |> C.variant1 "a" A assocIdDoc
      |> C.buildCustom


-- General / Shared

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


type alias PointDoc =
  { x : C.Ref Point C.Settable Int
  , y : C.Ref Point C.Settable Int
  , schema : C.Schema C.Nested Point
  }


pointDoc : PointDoc
pointDoc =
  C.record Point PointDoc
    |> C.field "x" .x C.int
    |> C.field "y" .y C.int
    |> C.build


type alias RectangleDoc =
  { x1 : C.Ref Rectangle C.Settable Int
  , y1 : C.Ref Rectangle C.Settable Int
  , x2 : C.Ref Rectangle C.Settable Int
  , y2 : C.Ref Rectangle C.Settable Int
  , schema : C.Schema C.Nested Rectangle
  }


rectDoc : RectangleDoc
rectDoc =
  C.record Rectangle RectangleDoc
    |> C.field "x1" .x1 C.int
    |> C.field "y1" .y1 C.int
    |> C.field "x2" .x2 C.int
    |> C.field "y2" .y2 C.int
    |> C.build


type alias ExpansionDoc =
  { schema : C.Schema (C.Variants Expansion) Expansion
  }


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
