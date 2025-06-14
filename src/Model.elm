module Model exposing (..)

import Dict exposing (Dict)
import Time



type alias Model =
  { activeMap : Id
  , maps : Maps
  , items : Items
  , selection : List Id   -- transient
  , dragState : DragState -- transient
  , nextId : Id
  }


type alias Maps = Dict Id Map
type alias Map = Dict Id MapEntry


type MapEntry
  = TopicEntry Point
  | AssocEntry


type alias Items = Dict Id Item


type Item
  = Topic TopicInfo
  | Assoc AssocInfo


type alias TopicInfo =
  { id : Id
  , color : Color
  }


type alias AssocInfo =
  { id : Id
  , player1 : Id
  , role1 : String
  , player2 : Id
  , role2 : String
  }


type alias Point =
  { x : Int
  , y : Int
  }


type alias Id = Int
type alias RoleType = String
type alias Class = String
type alias Delta = Point
type alias Color = Int


type DragState
  = NoDrag
  | WaitForStartTime Class Id Point
  | DragEngaged Time.Posix Class Id Point -- topic id, start point
  | WaitForEndTime Time.Posix Class Id Point
  | Drag DragMode Id Point (Maybe Id) -- topic id, last point, drop tartget


type DragMode
  = DragTopic
  | DrawAssoc


type Msg
  = AddTopic
  | Delete
  | Mouse MouseMsg
  | NoOp


type MouseMsg
  = Down -- mouse down somewhere
  | DownItem Class Id Point -- mouse down on an item where a drag can be engaged
  | Move Point
  | Up
  | Over Class Id
  | Time Time.Posix
