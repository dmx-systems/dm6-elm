module Feature.Search exposing (..)

import ModelParts exposing (Id, BoxId)



type alias Model =
  { text : String
  , menu : Menu
  }


init : Model
init =
  { text = ""
  , menu = Closed
  }


type Menu
  = Topics (List Id) (Maybe Id) -- hovered topic
  | RelTopics (List (Id, Id)) (Maybe (Id, Id)) -- hovered related topic (topic ID, assoc ID)
  | Closed


type Msg
  -- Search
  = Input String
  | FocusInput
  | HoverTopic Id
  | UnhoverTopic Id
  | ClickTopic Id
  -- Traverse
  | Traverse
  | HoverRelTopic (Id, Id)
  | UnhoverRelTopic (Id, Id)
  | ClickRelTopic (Id, Id)
  -- Fullscreen
  | Fullscreen BoxId
