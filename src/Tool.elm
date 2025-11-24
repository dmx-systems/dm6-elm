module Tool exposing (..)

import ModelHelper exposing (Id, BoxId)



type Msg
  -- Global Tools
  = AddTopic
  | AddBox
  | Undo
  | Redo
  | Import
  | Export
  -- Item Tools
  | ToggleDisplay Id BoxId -- TODO: make params Id BoxPath
  | Unbox BoxId BoxId -- TODO: make params BoxId BoxPath
  | Hide -- TODO: add params instead of operating on selection
  | Delete -- TODO: add params instead of operating on selection
