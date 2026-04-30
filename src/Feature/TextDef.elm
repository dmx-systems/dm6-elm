module Feature.TextDef exposing (Model, Msg(..), EditState(..), TopicImage, init)

import ModelBase exposing (..)

import Dict exposing (Dict)



type alias Model =
  { edit : EditState
  , measure : String
  , imageCache : Dict ImageId String -- Int -> blob: URL
  }


init : Model
init =
  { edit = NoEdit
  , measure = ""
  , imageCache = Dict.empty
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
  | ImageUrlResolved (ImageId, String)


type alias TopicImage =
  { topicId : TopicId
  , imageId : Id
  }
