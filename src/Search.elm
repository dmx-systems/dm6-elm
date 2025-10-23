module Search exposing (..)

import Model exposing (Id)



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
  | RelTopics (List (Id, Id)) (Maybe (Id, Id)) -- (topic ID, assoc ID)
  | Closed


type Msg
  -- Search
  = Input String
  | FocusInput
  | HoverTopic Id
  | UnhoverTopic Id
  | ClickTopic Id
  -- Traverse
  | ShowRelated
  | HoverRelTopic (Id, Id)
  | UnhoverRelTopic (Id, Id)
  | ClickRelTopic (Id, Id)
