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
  | ToggleDisplay Id BoxId -- TODO: make params Id BoxPath?
  | Unbox BoxId BoxId -- TODO: make params BoxId BoxPath?
  | Remove
  | Delete
