module Feature.Sel exposing (..)

import ModelParts exposing (Id, BoxPath)



type alias Model =
  { items : Selection }


init : Model
init =
  { items = [] }


type alias Selection = List (Id, BoxPath) -- TODO: make it a Set?
