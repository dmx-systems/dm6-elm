module Icon exposing (..)

import ModelHelper exposing (Id, IconName)



-- TYPES


type alias Model =
  { menu : Menu }


init : Model
init =
  { menu = Closed }


type Menu
  = Open
  | Closed


type Msg
  = OpenMenu
  | CloseMenu
  | SetIcon (Maybe IconName)
