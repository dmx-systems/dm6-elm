module Feature.SelDef exposing (..)

import ModelBase exposing (Target)



type alias Model =
  { items : Selection }


init : Model
init =
  { items = [] }


type alias Selection = List Target -- TODO: make it a Set?
