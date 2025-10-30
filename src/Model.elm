module Model exposing (..)

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
  , iconName : Maybe IconName -- serialzed as "icon"
  }


type alias AssocInfo =
  { id : Id
  , itemType : ItemType -- serialzed as "type", field can't be named "type", a reserved word
  , role1 : RoleType
  , player1 : Id
  , role2 : RoleType
  , player2 : Id
  }


type alias MapPath = List MapId


type alias Maps = Dict Id Map


type alias Map =
  { id : MapId
  , rect : Rectangle
  , items : MapItems
  }


type alias MapItems = Dict Id MapItem


type alias MapItem =
  { id : Id
  , parentAssocId : Id
  , hidden : Bool -- TODO: replace hidden/pinned by custom type: Hidden/Visible/Pinned?
  , pinned : Bool
  , props : MapProps
  }


type MapProps -- TODO: rename to "ItemProps"?
  = MapTopic TopicProps
  | MapAssoc AssocProps


type alias TopicProps =
  { pos : Point
  , size : Size -- TODO: really per-map?
  , displayMode : DisplayMode -- serialized as "display", TODO: rename to "display"?
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


type alias Selection = List (Id, MapPath) -- TODO: make it a Set?


type alias Id = Int
type alias MapId = Id
type alias AssocIds = Set Id
type alias Class = String -- a CSS class, e.g. "dmx-topic"
type alias ItemType = String -- a type URI, e.g. "dmx.association"
type alias RoleType = String -- a role type URI, e.g. "dmx.default"
type alias Delta = Point
type alias IconName = String -- name of feather icon, https://feathericons.com


type EditState
  = ItemEdit Id MapId
  | NoEdit


type EditMsg
  = EditStart
  | OnTextInput String
  | OnTextareaInput String
  | SetTopicSize Id MapId Size
  | EditEnd


type NavMsg
  = Fullscreen
  | Back
