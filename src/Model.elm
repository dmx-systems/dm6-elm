module Model exposing (..)

import Dict exposing (Dict)
import Time



type alias Items = Dict Id Item


type Item -- TODO: make it a record with "id" field, analogue MapItem?
  = Topic TopicInfo
  | Assoc AssocInfo


type alias TopicInfo =
  { id : Id
  , text : String
  , iconName : Maybe IconName
  }


type alias AssocInfo =
  { id : Id
  , itemType : ItemType -- can't be named "type", a reserved word
  , player1 : Id
  , role1 : RoleType
  , player2 : Id
  , role2 : RoleType
  }


type alias Maps = Dict Id Map
type alias MapItems = Dict Id MapItem


type alias Map =
  { id : MapId
  , items : MapItems
  , rect : Rectangle
  , parentMapId : MapId -- FIXME: ambiguous semantics? view context vs model?
  }


type alias MapItem =
  { id : Id
  , hidden : Bool -- TODO: replace hidden/pinned by custom type: Hidden/Visible/Pinned?
  , pinned : Bool
  , props : MapProps
  , parentAssocId : Id
  }


type MapProps
  = MapTopic TopicProps
  | MapAssoc AssocProps


type alias TopicProps =
  { pos : Point
  , size : Size
  , displayMode : DisplayMode
  }


type alias AssocProps =
  {}


type DisplayMode
  = Monad MonadDisplay
  | Container ContainerDisplay


type MonadDisplay
  = LabelOnly
  | Detail


type ContainerDisplay
  = BlackBox
  | WhiteBox
  | Unboxed


type alias Point =
  { x : Float
  , y : Float
  }


type alias Rectangle =
  { x1 : Float
  , y1 : Float
  , x2 : Float
  , y2 : Float
  }


type alias Size =
  { w : Float
  , h : Float
  }


type alias Selection = List (Id, MapId)


type alias Id = Int
type alias MapId = Id
type alias Class = String -- a CSS class, e.g. "dmx-topic"
type alias ItemType = String -- a type URI, e.g. "dmx.association"
type alias RoleType = String -- a role type URI, e.g. "dmx.default"
type alias Delta = Point
type alias IconName = String -- name of feather icon, https://feathericons.com


type EditState
  = ItemEdit Id MapId
  | NoEdit


type DragState
  = WaitForStartTime Class Id MapId Point -- start point (mouse)
  | DragEngaged Time.Posix Class Id MapId Point -- start point (mouse)
  | WaitForEndTime Time.Posix Class Id MapId Point -- start point (mouse)
  | Drag DragMode Id MapId Point Point (Maybe (Id, MapId)) -- orig topic pos, last point (mouse)
  | NoDrag


type DragMode
  = DragTopic
  | DrawAssoc


type EditMsg
  = EditStart
  | OnTextInput String
  | OnTextareaInput String
  | SetTopicSize Id MapId Size
  | EditEnd


type IconMenuMsg
  = Open
  | Close
  | SetIcon (Maybe IconName)


type MouseMsg
  = Down -- mouse down somewhere
  | DownItem Class Id MapId Point -- mouse down on an item where a drag can be engaged
  | Move Point
  | Up
  | Over Class Id MapId
  | Out Class Id MapId
  | Time Time.Posix


type NavMsg
  = Fullscreen
  | Back
