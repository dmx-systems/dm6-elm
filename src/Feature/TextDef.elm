module Feature.TextDef exposing (Model, Msg(..), EditState(..), TopicImage, init)

import ModelBase exposing (..)



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
  = Edit TopicId BoxPath
  | NoEdit


type Msg
  = OnTextInput String
  | OnTextareaInput String
  | GotTextSize Id SizeField Size
  | LeaveEdit
  | ImageFilePicked TopicImage


type alias TopicImage =
  { topicId : TopicId
  , imageId : Id
  }
