module AppModel exposing (..)

import Config exposing (homeMapName)
import Model exposing (..)
-- components
import IconMenu
import Mouse
import Search

import Dict
import Set
import UndoList exposing (UndoList)



type alias UndoModel = UndoList Model


type alias Model =
  { items : Items -- TODO: represent container content independent from maps?
  , maps : Maps
  , mapPath : MapPath
  , nextId : Id
  ----- transient -----
  , selection : Selection
  , editState : EditState
  , measureText : String
  -- components
  , mouse : Mouse.Model
  , search : Search.Model
  , iconMenu : IconMenu.Model
  }


default : Model
default =
  { items = Dict.singleton 0 <| Item 0 (Topic (TopicInfo 0 homeMapName Nothing)) Set.empty
  , maps = Dict.singleton 0 -- map 0 is the "home map"
    <| Map 0 (Rectangle 0 0 0 0) Dict.empty
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


resetTransientState : Model -> Model
resetTransientState model =
  { model
  ----- transient -----
  | selection = default.selection
  , editState = default.editState
  , measureText = default.measureText
  -- components
  , mouse = default.mouse
  , search = default.search
  , iconMenu = default.iconMenu
  }


type Msg
  = AddTopic
  | MoveTopicToMap Id MapId Point Id MapPath Point -- start point, random point (for target)
  | SwitchDisplay DisplayMode
  | Edit EditMsg
  | Nav NavMsg
  | Hide
  | Delete
  | Undo
  | Redo
  | Import
  | Export
  | NoOp
  -- components
  | Mouse Mouse.Msg
  | Search Search.Msg
  | IconMenu IconMenu.Msg
