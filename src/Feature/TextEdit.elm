module Feature.TextEdit exposing (..)

import ModelParts exposing (Id, BoxId, Size)



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
  = EditStart
  | OnTextInput String
  | OnTextareaInput String
  | SetTopicSize Id BoxId Size
  | EditEnd
