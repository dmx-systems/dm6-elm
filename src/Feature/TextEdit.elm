module Feature.TextEdit exposing (..)

import ModelParts exposing (Id, BoxPath, Size, SizeField)

import Dict exposing (Dict)



type alias Model =
  { state : EditState
  , measureText : String
  , imageCache : Dict String String -- app: key -> blob: URL
  }


init : Model
init =
  { state = NoEdit
  , measureText = ""
  , imageCache = Dict.empty
  }


type EditState
  = ItemEdit Id BoxPath
  | NoEdit


type Msg
  = OnTextInput String
  | OnTextareaInput String
  | GotTextSize Id SizeField Size
  | EditEnd -- TODO: drop it, expose API instead
