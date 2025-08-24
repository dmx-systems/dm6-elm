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
  , menu = ResultClosed
  }


type ResultMenu
  = ResultOpen (Maybe Id) -- hovered topic
  | ResultClosed


type SearchMsg
  = SearchInput String
  | SearchFocus
  | OverItem Id
  | OutItem Id
  | ClickItem Id
