module SyncModel exposing (SyncModel, init)

import ModelBase exposing (..)

import Dict exposing (Dict)



-- TYPES


type alias SyncModel =
  { topics : Dict Id Topic
  , assocs : Dict Id Assoc
  , boxes : Dict Id Box
  , itemSets: Dict Id ItemSet
  -- TODO
  }


init : SyncModel
init =
  { topics = Dict.empty
  , assocs = Dict.empty
  , boxes = Dict.empty
  , itemSets = Dict.empty
  }
