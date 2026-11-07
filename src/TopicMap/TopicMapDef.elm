module TopicMap.TopicMapDef exposing (Model, SyncModel, TopicMap, MapTopic, DragState(..),
  DragMode(..), Msg(..), init, initSync, topicMapsDict, topicMapDoc, mapTopicsDict, mapTopicDoc)

import Feature.CrdtBase exposing (..)
import ModelBase exposing (..)

import Crdt as C

import Dict exposing (Dict)
import Time



type alias Model =
  {
  -- State machine to initiate a drag from a TopicMap box.
  -- Available while a drag that started from a TopicMap box is active.
    dragState : Maybe DragState -- transient
  }


type alias SyncModel =
  Dict Id TopicMap


init : Model
init =
  { dragState = Nothing -- transient
  }


initSync : SyncModel
initSync =
  Dict.empty


type alias TopicMap =
  { id : BoxId
  , rect : Rectangle
  , scroll : Point
  , topics : Dict Id MapTopic
  }


type alias MapTopic =
  { id : TopicId
  , pos : Point
  , expansion : Expansion -- "effective expansion", computed/transient,
                          -- used for both limbo-rendering and auto-sizing
  }


type DragState
  = WaitForStartTime
  | DragEngaged Time.Posix    -- start time, entered once start time arrives
  | WaitForEndTime Time.Posix -- start time (buffered), entered on 1st move (once engaged)
  | Drag DragMode             -- entered once end time arrives


type DragMode
  = DragTopic Point -- original topic position
  | DraftAssoc


type Msg
  = AssocClicked AssocId BoxPath
  | GotTime Time.Posix
  | GotRandomPos TopicId BoxId Point



-- CRDT SCHEMA


topicMapsDict =
  C.dict topicMapDoc


mapTopicsDict =
  C.dict mapTopicDoc


type alias TopicMapDoc =
  { id : C.Ref TopicMap (C.Variants BoxId) BoxId
  , rect : C.Ref TopicMap C.Nested Rectangle
  , scroll : C.Ref TopicMap C.Nested Point
  , topics : C.Ref TopicMap (C.DictK C.Nested MapTopic) (Dict Id MapTopic)
  , schema : C.Schema C.Nested TopicMap
  }


type alias MapTopicDoc =
  { id : C.Ref MapTopic (C.Variants TopicId) TopicId
  , pos : C.Ref MapTopic C.Nested Point
  , expansion : C.Ref MapTopic (C.Variants Expansion) Expansion
  , schema : C.Schema C.Nested MapTopic
  }


topicMapDoc : TopicMapDoc
topicMapDoc =
  C.record TopicMap TopicMapDoc
    |> C.field "id" .id boxIdDoc
    |> C.field "rect" .rect rectDoc
    |> C.field "scroll" .scroll pointDoc
    |> C.field "topics" .topics mapTopicsDict
    |> C.build


mapTopicDoc : MapTopicDoc
mapTopicDoc =
  C.record MapTopic MapTopicDoc
    |> C.field "id" .id topicIdDoc
    |> C.field "pos" .pos pointDoc
    |> C.field "expansion" .expansion expansionDoc
    |> C.build
