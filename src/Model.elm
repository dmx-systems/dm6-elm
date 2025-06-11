module Model exposing (..)

import Dict exposing (Dict)


type alias Model =
  { items: Dict Id Item
  , nextId : Id
  }


type Item
  = Topic Id Point
  | Assoc Id RoleType Id RoleType


type alias Id = Int
type alias RoleType = String


type alias Point =
  { x : Int
  , y : Int
  }


type Msg
  = AddTopic
