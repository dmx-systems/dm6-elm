module SyncModel exposing (SyncModel, bootstrap)

import ModelBase exposing (..)
-- box renderers
import TopicMap.TopicMapDef as TopicMapDef
import TopicList.TopicListDef as TopicListDef

import Dict exposing (Dict)



-- TYPES


type alias SyncModel =
  { topics : Dict Id Topic
  , assocs : Dict Id Assoc
  , boxes : Dict Id Box
  , rootBoxId : BoxId
  , itemSets : Dict Id ItemSet
  -- box renderers
  , topicMap : TopicMapDef.SyncModel
  , topicList : TopicListDef.SyncModel
  }


bootstrap : SyncModel
bootstrap =
  { topics = Dict.empty
  , assocs = Dict.empty
  , boxes = Dict.empty
  , rootBoxId = BoxId (TopicId "") -- Set while cache materialization (Main.bootstrapContent)
  , itemSets = Dict.empty
  -- box renderers
  , topicMap = TopicMapDef.initSync
  , topicList = TopicListDef.initSync
  }
