module Feature.Tool exposing (..)

import ModelParts exposing (Id, BoxId)



type Msg
  -- Global Tools
  = Home
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
  | Fullscreen BoxId  -- box only
  | Unbox BoxId BoxId -- box only
  | ToggleDisplay Id BoxId
  -- Text Tools
  | Image Id
  | LeaveEdit
