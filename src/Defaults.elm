module Defaults exposing
    ( editState
    , iconMenu
    , measureText
    , mouse
    , search
    , selection
    )

import IconMenu exposing (..)
import Model exposing (EditState(..), Selection)
import Mouse exposing (..)
import Search exposing (..)


selection : Selection
selection =
    []


editState : EditState
editState =
    NoEdit


measureText : String
measureText =
    ""


mouse : MouseModel
mouse =
    Mouse.init


search : SearchModel
search =
    Search.init


iconMenu : IconMenuModel
iconMenu =
    IconMenu.init
