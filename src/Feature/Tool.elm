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
  | Delete
  | Remove
  | Fullscreen BoxId
  | Unbox BoxId BoxId
  | ToggleDisplay Id BoxId
  -- Text Tools
  | Image Id
  | Link
