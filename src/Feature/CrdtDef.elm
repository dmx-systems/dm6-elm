module Feature.CrdtDef exposing (Model, bootstrap, init, decodeDoc, refs,
  topicIconRef, topicTextRef, topicViewSizeRef, topicEditorSizeRef, topicAssocIdsRef,
  boxTopicsRef, expansionRef, boxRendererRef, itemSetItemsRef)

import Console
import Feature.CrdtBase exposing (..)
import ModelBase exposing (..)
import Renderer exposing (Renderer)
import SyncModel exposing (SyncModel)
-- box renderers
import TopicMap.TopicMapDef as TopicMapDef exposing (TopicMap)
import TopicList.TopicListDef as TopicListDef exposing (TopicList)

import Crdt as C
import Crdt.Doc as Doc exposing (Doc, Version)
import Crdt.Id

import Dict exposing (Dict)
import Json.Encode as E



-- FEATURE MODULE


type alias Model =
  { doc : Doc SyncModel
  , lastStored : Version
  , editStartVersion : Maybe Version
  }


bootstrap : Model
bootstrap =
  init freshDoc


init : Doc SyncModel -> Model
init doc =
  { doc = doc
  , lastStored = Doc.version doc
  , editStartVersion = Nothing
  }


decodeDoc : E.Value -> Maybe (Doc SyncModel, SyncModel)
decodeDoc ops =
  freshDoc
    |> Doc.decodeInto ops
    |> Result.mapError (Console.logError "Feature.CrdtDef.decodeDoc" "Decode error")
    |> Result.andThen
        (\doc ->
          Doc.read doc
            |> Result.map (\docModel -> Just (doc, docModel))
            |> Result.mapError
              (Doc.readErrorToString
                >> Console.logError "Feature.CrdtDef.decodeDoc" "Schema read error"
              )
        )
    |> Result.toMaybe
    |> Maybe.withDefault Nothing


freshDoc : Doc SyncModel
freshDoc =
  C.init (Crdt.Id.replica "my-replica") modelDoc.schema -- TODO: replica ID



-- CRDT SCHEMA


type alias ModelDoc =
  { topics : C.Ref SyncModel (C.DictK C.Nested Topic) (Dict Id Topic)
  , assocs : C.Ref SyncModel (C.DictK C.Nested Assoc) (Dict Id Assoc)
  , boxes : C.Ref SyncModel (C.DictK C.Nested Box) (Dict Id Box)
  , rootBoxId : C.Ref SyncModel (C.Variants BoxId) BoxId
  , itemSets : C.Ref SyncModel (C.DictK C.Nested ItemSet) (Dict Id ItemSet)
  -- box renderers
  , topicMap : C.Ref SyncModel (C.DictK C.Nested TopicMap) (Dict Id TopicMap)
  , topicList : C.Ref SyncModel (C.DictK C.Nested TopicList) (Dict Id TopicList)
  --
  , schema : C.Schema C.Nested SyncModel
  }


modelDoc : ModelDoc
modelDoc =
  C.record SyncModel ModelDoc
    |> C.field "topics" .topics topicsDict
    |> C.field "assocs" .assocs assocsDict
    |> C.field "boxes" .boxes boxesDict
    |> C.field "rootBoxId" .rootBoxId boxIdDoc
    |> C.field "itemSets" .itemSets itemSetsDict
    -- box renderers
    |> C.field "topicMap" .topicMap TopicMapDef.topicMapsDict
    |> C.field "topicList" .topicList TopicListDef.topicListsDict
    --
    |> C.build


refs : ModelDoc
refs =
  modelDoc


topicsDict :
  C.Crdt
    (C.DictK C.Nested Topic) (Dict Id Topic)
    { key :
        Id
          -> C.Ref SyncModel (C.DictK C.Nested Topic) (Dict Id Topic)
          -> C.Ref SyncModel C.Nested Topic
    }
topicsDict =
  C.dict topicDoc


assocsDict :
  C.Crdt
    (C.DictK C.Nested Assoc) (Dict Id Assoc)
    { key :
        Id
          -> C.Ref SyncModel (C.DictK C.Nested Assoc) (Dict Id Assoc)
          -> C.Ref SyncModel C.Nested Assoc
    }
assocsDict =
  C.dict assocDoc


boxesDict :
  C.Crdt
    (C.DictK C.Nested Box) (Dict Id Box)
    { key :
        Id
          -> C.Ref SyncModel (C.DictK C.Nested Box) (Dict Id Box)
          -> C.Ref SyncModel C.Nested Box
    }
boxesDict =
  C.dict boxDoc


itemSetsDict :
  C.Crdt
    (C.DictK C.Nested ItemSet) (Dict Id ItemSet)
    { key :
        Id
          -> C.Ref SyncModel (C.DictK C.Nested ItemSet) (Dict Id ItemSet)
          -> C.Ref SyncModel C.Nested ItemSet
    }
itemSetsDict =
  C.dict itemSetDoc



-- REF COMPOSITION


topicRef : Id -> C.Ref SyncModel C.Nested Topic
topicRef id =
  refs.topics
    |> topicsDict.key id


topicIconRef : Id -> C.Ref SyncModel (C.Variants (Maybe Icon)) (Maybe Icon)
topicIconRef id =
  topicRef id
    |> C.at topicDoc.icon


topicTextRef : Id -> C.Ref SyncModel C.Settable String
topicTextRef id =
  topicRef id
    |> C.at topicDoc.text


topicSizeRef : Id -> C.Ref SyncModel C.Nested TextSize
topicSizeRef id =
  topicRef id
    |> C.at topicDoc.size


topicViewSizeRef : Id -> C.Ref SyncModel C.Nested Size
topicViewSizeRef id =
  topicSizeRef id
    |> C.at textSizeDoc.view


topicEditorSizeRef : Id -> C.Ref SyncModel C.Nested Size
topicEditorSizeRef id =
  topicSizeRef id
    |> C.at textSizeDoc.editor


topicAssocIdsRef : Id -> C.Ref SyncModel (C.ListK C.Fixed (C.Variants AssocId) AssocId)
                                                                                  (List AssocId)
topicAssocIdsRef id =
  topicRef id
    |> C.at topicDoc.assocIds


--

boxRef : Id -> C.Ref SyncModel C.Nested Box
boxRef id =
  refs.boxes
    |> boxesDict.key id


itemSetRef : Id -> C.Ref SyncModel C.Nested ItemSet
itemSetRef id =
  refs.itemSets
    |> itemSetsDict.key id


itemSetItemsRef : Id -> C.Ref SyncModel (C.ListK C.Fixed C.Nested SetItem) (List SetItem)
itemSetItemsRef id =
  itemSetRef id
    |> C.at itemSetDoc.items


boxTopicsRef : Id -> C.Ref SyncModel (C.DictK C.Nested BoxTopic) (Dict Id BoxTopic)
boxTopicsRef id =
  boxRef id
    |> C.at boxDoc.topics


boxTopicRef : Id -> Id -> C.Ref SyncModel C.Nested BoxTopic
boxTopicRef topicId boxId =
  boxTopicsRef boxId
    |> boxTopicsDict.key topicId


expansionRef : Id -> Id -> C.Ref SyncModel (C.Variants Expansion) Expansion
expansionRef topicId boxId =
  boxTopicRef topicId boxId
    |> C.at boxTopicDoc.expansion


boxRendererRef : Id -> C.Ref SyncModel (C.Variants Renderer) Renderer
boxRendererRef id =
  boxRef id
    |> C.at boxDoc.renderer
