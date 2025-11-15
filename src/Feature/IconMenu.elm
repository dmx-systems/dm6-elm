module Feature.IconMenu exposing (..)

import Model exposing (IconName)



type alias Model =
  { open : Bool }


init : Model
init =
  { open = False }


type Msg
  = Open
  | Close
  | SetIcon (Maybe IconName)
