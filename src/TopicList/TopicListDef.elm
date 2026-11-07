module TopicList.TopicListDef exposing (Model, SyncModel, TopicList, DropMode(..), Targets,
  init, initSync, topicListsDict, topicListDoc)

import Feature.CrdtBase exposing (..)
import ModelBase exposing (..)

import Crdt as C

import Array exposing (Array)
import Dict exposing (Dict)



type alias Model =
  {
  -- Position of dragging list item.
  -- Available while a drag that started from a TopicList box is active.
    dragPos : Maybe Point -- transient
  -- Accepted drop location within a TopicList box.
  -- Note: the drag has not necessarily started from a TopicList box (but another renderer).
  , dropMode : Maybe DropMode -- transient
  }


type alias SyncModel =
  Dict Id TopicList


init : Model
init =
  { dragPos = Nothing
  , dropMode = Nothing
  }


initSync : SyncModel
initSync =
  Dict.empty


type alias TopicList =
  { id : BoxId
  , order : List TopicId
  , size : Size
  }


type DropMode
  = Drop
  | InsertBefore


type alias Targets = Array (Level, Target)



-- CRDT SCHEMA


topicListsDict =
  C.dict topicListDoc


type alias TopicListDoc =
  { id : C.Ref TopicList (C.Variants BoxId) BoxId                  -- TODO: make order C.Movable
  , order : C.Ref TopicList (C.ListK C.Fixed (C.Variants TopicId) TopicId) (List TopicId)
  , size : C.Ref TopicList C.Nested Size
  , schema : C.Schema C.Nested TopicList
  }


topicListDoc : TopicListDoc
topicListDoc =
  C.record TopicList TopicListDoc
    |> C.field "id" .id boxIdDoc
    |> C.field "order" .order (C.list topicIdDoc)
    |> C.field "size" .size sizeDoc
    |> C.build
