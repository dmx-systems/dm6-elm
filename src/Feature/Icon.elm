module Feature.Icon exposing (..)

import ModelParts exposing (Icon)



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
  | SetIcon (Maybe Icon)
