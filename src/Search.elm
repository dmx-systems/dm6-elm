module Search exposing (..)

import Model exposing (..)



type alias Model =
  { searchText : String
  , searchResult : List Id -- topic Ids
  , searchMenu : ResultMenu
  }


init : Model
init =
  { searchText = ""
  , searchResult = []
  , searchMenu = ResultClosed
  }
