module Feature.Tool exposing (..)

import ModelParts exposing (Id, BoxId)



type alias Model =
  { menu : Bool
  , lineStyle : LineStyle
  }


init : Model
init =
  { menu = False
  , lineStyle = Cornered
  }


type LineStyle
  = Cornered
  | Straight


type Msg
  -- Global Tools
  = Home
  | Menu
  | Set LineStyle
  | Import
  | Export
  -- Map Tools
  | AddTopic
  | AddBox
  | Undo
  | Redo
  -- Item Tools
  | Edit
  | Icon
  | Traverse
  | Delete
  | Remove
  | Fullscreen BoxId -- box only
  | Unbox BoxId BoxId -- box only
  | ToggleDisplay Id BoxId
  -- Text Tools
  | Image Id
  | LeaveEdit
