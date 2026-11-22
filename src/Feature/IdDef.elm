module Feature.IdDef exposing (..)

import ModelBase exposing (..)



type alias Model =
  { pool : List Id }


init : Model
init =
  { pool = [] }


type Msg
  = GotIds (List Id)
