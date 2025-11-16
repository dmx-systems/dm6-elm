module IconMenu exposing (..)

import ModelHelper exposing (IconName)



type alias Model =
  { open : Bool }


init : Model
init =
  { open = False }


type Msg
  = Open
  | Close
  | SetIcon (Maybe IconName)
