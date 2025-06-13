module Model exposing (..)

import Dict exposing (Dict)



type alias Model =
  { items : Items
  , selection: List Id    -- transient
  , dragState : DragState -- transient
  , nextId : Id
  }


type alias Items = Dict Id Item


type Item
  = Topic TopicInfo
  | Assoc AssocInfo


type alias TopicInfo =
  { id : Id
  , pos : Point
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
  | DragEngaged Class Id Point    -- topic id, start point
  | DragTopic Id Point (Maybe Id) -- topic id, last point, drop tartget


type Msg
  = AddTopic
  | Mouse MouseMsg


type MouseMsg
  = Down -- mouse down somewhere
  | DownItem Class Id Point -- mouse down on an item where a drag can be engaged
  | Move Point
  | Up
  | Over Class Id
