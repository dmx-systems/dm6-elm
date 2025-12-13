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
  | Delete
  | Remove
  | Fullscreen BoxId
  | Unbox BoxId BoxId -- TODO: make params BoxId BoxPath?
  | ToggleDisplay Id BoxId -- TODO: make params Id BoxPath?
  -- Text Tools
  | Image
  | Link
