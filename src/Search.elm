module Search exposing (..)

import Model exposing (Id)



type alias Model =
  { text : String
  , result : List Id -- topic Ids
  , menu : ResultMenu
  }


init : Model
init =
  { text = ""
  , result = []
  , menu = Closed
  }


type ResultMenu
  = Open (Maybe Id) -- hovered topic
  | Closed


type Msg
  = Input String
  | FocusInput
  | HoverItem Id
  | UnhoverItem Id
  | ClickItem Id
