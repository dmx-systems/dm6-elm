module Model exposing (..)

import Config as C
import ModelHelper exposing (..)
-- feature modules
import IconMenu
import Mouse
import Search
import TextEdit

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
  -- feature modules
  , edit : TextEdit.Model
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
  -- feature modules
  , edit = TextEdit.init
  , mouse = Mouse.init
  , search = Search.init
  , iconMenu = IconMenu.init
  }


initTransient : Model -> Model
initTransient model =
  { model
  ----- transient -----
  | selection = init.selection
  -- feature modules
  , edit = init.edit
  , mouse = init.mouse
  , search = init.search
  , iconMenu = init.iconMenu
  }


type Msg
  = AddTopic
  | AddBox
  | MoveTopicToBox Id BoxId Point Id BoxPath Point -- start point, random point (for target)
  | SwitchDisplay DisplayMode
  | Nav NavMsg
  | Hide
  | Delete
  | Undo
  | Redo
  | Import
  | Export
  | NoOp
  -- feature modules
  | Edit TextEdit.Msg
  | Mouse Mouse.Msg
  | Search Search.Msg
  | IconMenu IconMenu.Msg
