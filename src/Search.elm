module Search exposing (..)

import Model exposing (Id)


type alias SearchModel =
    { text : String
    , result : List Id -- topic Ids
    , menu : ResultMenu
    }


init : SearchModel
init =
    { text = ""
    , result = []
    , menu = Closed
    }


type ResultMenu
    = Open (Maybe Id) -- hovered topic
    | Closed


type SearchMsg
    = Input String
    | FocusInput
    | HoverItem Id
    | UnhoverItem Id
    | ClickItem Id
