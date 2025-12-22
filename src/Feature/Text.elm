module Feature.Text exposing (..)

import ModelParts exposing (Id, ImageId, BoxPath, Size, SizeField)



type alias Model =
  { edit : EditState
  , measure : String
  }


init : Model
init =
  { edit = NoEdit
  , measure = ""
  }


type EditState
  = Edit Id BoxPath
  | NoEdit


type Msg
  = OnTextInput String
  | OnTextareaInput String
  | GotTextSize Id SizeField Size
  | LeaveEdit
  | ImageFilePicked (Id, ImageId)
