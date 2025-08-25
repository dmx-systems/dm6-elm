module AppModel exposing (..)

import Model exposing (..)
-- components
import IconMenu exposing (IconMenuModel, IconMenuMsg)
import Mouse exposing (MouseModel, MouseMsg)
import Search exposing (SearchModel, SearchMsg)

import Dict



type alias Model =
  { items : Items -- the knowledge base
  , maps : Maps -- the views
  , mapPath : List MapId
  , nextId : Id
  ----- transient -----
  , selection : Selection
  , editState : EditState
  , measureText : String
  -- components
  , mouse : MouseModel
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
  , measureText = ""
  -- components
  , mouse = Mouse.init
  , search = Search.init
  , iconMenu = IconMenu.init
  }


type Msg
  = AddTopic
  | MoveTopicToMap Id MapId Point Id MapId Point -- start point, random point (for target)
  | SwitchDisplay DisplayMode
  | Edit EditMsg
  | Nav NavMsg
  | Hide
  | Delete
  | NoOp
  -- components
  | Mouse MouseMsg
  | Search SearchMsg
  | IconMenu IconMenuMsg
