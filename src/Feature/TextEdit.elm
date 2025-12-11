module Feature.TextEdit exposing (..)

import ModelParts exposing (Id, BoxId, Size, SizeField)



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
  = ItemEdit Id BoxId
  | NoEdit


type Msg
  = OnTextInput String
  | OnTextareaInput String
  | GotTextSize Id SizeField Size
  | EditEnd
