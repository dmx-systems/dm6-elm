module Feature.Tool exposing (..)

import ModelParts exposing (Id, BoxId)



type alias Model =
  { menu : Bool }


init : Model
init =
  { menu = False }


type Msg
  -- Global Tools
  = Home
  | Menu
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
