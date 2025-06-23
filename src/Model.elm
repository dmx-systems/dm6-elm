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
  , itemType : ItemType
  , player1 : Id
  , role1 : RoleType
  , player2 : Id
  , role2 : RoleType
  }


type alias Maps = Dict Id Map
type alias ViewItems = Dict Id ViewItem


type alias TransferFunc = ViewItems -> ViewItems -> Model -> ViewItems


type alias Map =
  { id : MapId
  , parent : MapId
  , items : ViewItems
  }


type alias ViewItem =
  { id : Id
  , hidden : Bool
  , viewProps : ViewProps
  , mapAssocId : Id
  }


type ViewProps
  = ViewTopic TopicProps
  | ViewAssoc AssocProps


type alias TopicProps =
  { pos : Point
  , displayMode : Maybe DisplayMode -- only set for container topics
  }


type alias AssocProps =
  {}


type DisplayMode
  = BlackBox
  | WhiteBox
  | Unboxed


type alias Point =
  { x : Int
  , y : Int
  }


type alias Id = Int
type alias MapId = Id
type alias Class = String -- a CSS class, e.g. "dmx-topic"
type alias ItemType = String
type alias RoleType = String
type alias Selection = List (Id, MapId)
type alias Delta = Point
type alias Color = Int


type DragState
  = NoDrag
  | WaitForStartTime Class Id MapId Point
  | DragEngaged Time.Posix Class Id MapId Point -- topic id, start point
  | WaitForEndTime Time.Posix Class Id MapId Point
  | Drag DragMode Id MapId Point (Maybe (Id, MapId)) -- topic id, last point, drop tartget


type DragMode
  = DragTopic
  | DrawAssoc


type Msg
  = CreateTopic
  | MoveTopicToMap Id MapId Id MapId Point
  | Set (Maybe DisplayMode)
  | Delete
  | Mouse MouseMsg
  | NoOp


type MouseMsg
  = Down -- mouse down somewhere
  | DownItem Class Id MapId Point -- mouse down on an item where a drag can be engaged
  | Move Point
  | Up
  | Over Class Id MapId
  | Out Class Id MapId
  | Time Time.Posix
