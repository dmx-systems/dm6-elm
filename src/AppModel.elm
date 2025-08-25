module AppModel exposing (..)

import Model exposing (..)
-- components
import IconMenu exposing (IconMenuModel, IconMenuMsg)
import Search exposing (SearchModel, SearchMsg)

import Dict



type alias Model =
  { items : Items
  , maps : Maps
  , mapPath : List MapId
  , nextId : Id
  ----- transient -----
  , selection : Selection
  , editState : EditState
  , dragState : DragState
  , measureText : String
  -- components
  , search : SearchModel
  , iconMenu : IconMenuModel
  }


default : Model
default =
  { items = Dict.empty
  , maps = Dict.singleton 0 -- map 0 is the "home map", it has no corresponding topic
    <| Map 0 Dict.empty (Rectangle 0 0 0 0) -1 -- parentMapId = -1
  , mapPath = [0]
  , nextId = 1
  ----- transient -----
  , selection = []
  , editState = NoEdit
  , dragState = NoDrag
  , measureText = ""
  -- components
  , search = Search.init
  , iconMenu = IconMenu.init
  }


type Msg
  = AddTopic
  | MoveTopicToMap Id MapId Point Id MapId Point -- start point, random point (for target)
  | SwitchDisplay DisplayMode
  | Search SearchMsg
  | Edit EditMsg
  | IconMenu IconMenuMsg
  | Mouse MouseMsg
  | Nav NavMsg
  | Hide
  | Delete
  | NoOp
