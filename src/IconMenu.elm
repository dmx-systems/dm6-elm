module IconMenu exposing (..)

import Model exposing (IconName)


type alias IconMenuModel =
    { open : Bool }


init : IconMenuModel
init =
    { open = False }


type IconMenuMsg
    = Open
    | Close
    | SetIcon (Maybe IconName)
