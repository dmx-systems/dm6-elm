module Feature.TextEdit exposing (..)

import ModelParts exposing (Id, BoxPath, Size, SizeField)



type alias Model =
  { state : EditState
  , measureText : String
  }


init : Model
init =
  { state = NoEdit
  , measureText = ""
  }


type EditState
  = ItemEdit Id BoxPath
  | NoEdit


type Msg
  = OnTextInput String
  | OnTextareaInput String
  | GotTextSize Id SizeField Size
  | LeaveEdit
