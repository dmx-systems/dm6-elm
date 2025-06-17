module Model exposing (..)

import Dict exposing (Dict)
import Time



type alias Model =
  { items : Items
  , maps : Maps
  , activeMap : MapId
  , selection : Selection -- transient
  , dragState : DragState -- transient
  , nextId : Id
  }


type alias Maps = Dict Id Map


type alias Map =
  { id : MapId
  , items : Dict Id ViewItem
  }


type ViewItem
  = ViewTopic TopicProps
  | ViewAssoc AssocProps


type alias TopicProps =
  { id : Id
  , pos : Point
  , expanded : Bool
  }


type alias AssocProps =
  { id : Id }


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
type alias MapId = Id
type alias Selection = List (Id, MapId)
type alias RoleType = String
type alias Class = String
type alias Delta = Point
type alias Color = Int


type DragState
  = NoDrag
  | WaitForStartTime Class Id MapId Point
  | DragEngaged Time.Posix Class Id MapId Point -- topic id, start point
  | WaitForEndTime Time.Posix Class Id MapId Point
  | Drag DragMode Id MapId Point (Maybe Id) -- topic id, last point, drop tartget


type DragMode
  = DragTopic
  | DrawAssoc


type Msg
  = AddTopic
  | Expand Bool
  | Delete
  | Mouse MouseMsg
  | NoOp


type MouseMsg
  = Down -- mouse down somewhere
  | DownItem Class Id MapId Point -- mouse down on an item where a drag can be engaged
  | Move Point
  | Up
  | Over Class Id
  | Out Class Id
  | Time Time.Posix
