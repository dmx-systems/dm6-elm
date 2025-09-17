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
  | Closed


type Msg
  = Input String
  | FocusInput
  | HoverItem Id
  | UnhoverItem Id
  | ClickItem Id
