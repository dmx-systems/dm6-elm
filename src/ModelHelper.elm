module ModelHelper exposing (..)

import Dict exposing (Dict)
import Set exposing (Set)



type alias Items = Dict Id Item


type alias Item =
  { id : Id
  , info : ItemInfo
  , assocIds : AssocIds
  }


type ItemInfo
  = Topic TopicInfo
  | Assoc AssocInfo


type alias TopicInfo =
  { id : Id
  , text : String
  , icon : Maybe Icon
  }


type alias AssocInfo =
  { id : Id
  , itemType : ItemType -- serialized as "type", field can't be named "type", a reserved word
  , role1 : RoleType
  , player1 : Id
  , role2 : RoleType
  , player2 : Id
  }


type alias BoxPath = List BoxId


type alias Boxes = Dict Id Box


type alias Box =
  { id : BoxId
  , rect : Rectangle
  , items : BoxItems
  }


type alias BoxItems = Dict Id BoxItem


type alias BoxItem =
  { id : Id
  , parentAssocId : Id -- TODO: drop it? Compute from Item's "assocIds" field instead?
  , hidden : Bool -- TODO: replace hidden/pinned by custom type: Hidden/Visible/Pinned?
  , pinned : Bool
  , props : ViewProps
  }


type ViewProps
  = TopicV TopicProps
  | AssocV AssocProps


type alias TopicProps =
  { pos : Point
  , size : Size -- TODO: really per-box?
  , displayMode : DisplayMode -- serialized as "display", TODO: rename to "display"?
  }


type alias AssocProps =
  {}


type DisplayMode
  = TopicD TopicDisplay
  | BoxD BoxDisplay


type TopicDisplay
  = LabelOnly
  | Detail


type BoxDisplay
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


type alias Id = Int
type alias BoxId = Id
type alias AssocIds = Set Id
type alias Class = String -- a CSS class, e.g. "dmx-topic"
type alias ItemType = String -- a type URI, e.g. "dmx.association"
type alias RoleType = String -- a role type URI, e.g. "dmx.default"
type alias Delta = Point
type alias Icon = String -- name of feather icon, https://feathericons.com
