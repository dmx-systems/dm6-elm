module Feature.Icon exposing (..)

import ModelParts exposing (Icon)



-- TYPES


type alias Model =
  { picker : Picker }


init : Model
init =
  { picker = Closed }


type Picker
  = Open
  | Closed


type Msg
  = IconSelected (Maybe Icon)
