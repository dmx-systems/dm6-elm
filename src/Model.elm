module Model exposing (..)

import Config as C
import ModelHelper exposing (..)
-- feature modules
import IconMenu
import Mouse
import Search

import Dict
import Set
import UndoList exposing (UndoList)



type alias UndoModel = UndoList Model


type alias Model =
  { items : Items
  , boxes : Boxes
  , boxPath : BoxPath
  , nextId : Id
  ----- transient -----
  , selection : Selection
  , editState : EditState
  , measureText : String
  -- feature modules
  , mouse : Mouse.Model
  , search : Search.Model
  , iconMenu : IconMenu.Model
  }


init : Model
init =
  { items = Dict.singleton 0 <| Item 0 (Topic (TopicInfo 0 C.rootBoxName Nothing)) Set.empty
  , boxes = Dict.singleton 0 -- box 0 is the "root box"
    <| Box 0 (Rectangle 0 0 0 0) Dict.empty
  , boxPath = [0]
  , nextId = 1
  ----- transient -----
  , selection = []
  , editState = NoEdit
  , measureText = ""
  -- feature modules
  , mouse = Mouse.init
  , search = Search.init
  , iconMenu = IconMenu.init
  }


initTransient : Model -> Model
initTransient model =
  { model
  ----- transient -----
  | selection = init.selection
  , editState = init.editState
  , measureText = init.measureText
  -- feature modules
  , mouse = init.mouse
  , search = init.search
  , iconMenu = init.iconMenu
  }


type Msg
  = AddTopic
  | AddBox
  | MoveTopicToBox Id BoxId Point Id BoxPath Point -- start point, random point (for target)
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
  -- feature modules
  | Mouse Mouse.Msg
  | Search Search.Msg
  | IconMenu IconMenu.Msg
