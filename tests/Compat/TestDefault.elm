module Compat.TestDefault exposing (defaultModel)

import AppModel exposing (Model)
import Json.Encode as E
import Main


defaultModel : Model
defaultModel =
    Tuple.first (Main.init E.null)
