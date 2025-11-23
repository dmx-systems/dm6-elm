module Model exposing (..)

import Config as C
import ModelHelper exposing (..)
-- feature modules
import Icon
import Mouse
import Search
import Selection
import TextEdit

import Dict
import Set



type alias Model =
  { items : Items
  , boxes : Boxes
  , boxPath : BoxPath
  , nextId : Id
  ----- transient -----
  -- feature modules
  , edit : TextEdit.Model
  , mouse : Mouse.Model
  , search : Search.Model
  , icon : Icon.Model
  , selection : Selection.Model
  }


init : Model
init =
  { items = Dict.singleton 0 <| Item 0 (Topic (TopicInfo 0 C.rootBoxName Nothing)) Set.empty
  , boxes = Dict.singleton 0 -- box 0 is the "root box"
    <| Box 0 (Rectangle 0 0 0 0) Dict.empty
  , boxPath = [0]
  , nextId = 1
  ----- transient -----
  -- feature modules
  , edit = TextEdit.init
  , mouse = Mouse.init
  , search = Search.init
  , icon = Icon.init
  , selection = Selection.init
  }


initTransient : Model -> Model
initTransient model =
  { model
  ----- transient -----
  -- feature modules
  | edit = init.edit
  , mouse = init.mouse
  , search = init.search
  , icon = init.icon
  , selection = init.selection
  }


type Msg
  = AddTopic
  | AddBox
  | AddAssoc Id Id BoxId
  | MoveTopicToBox Id BoxId Point Id BoxPath Point -- start point, random point (for target)
  | DraggedTopic
  | ClickedItem Id BoxPath
  | ClickedBackground
  | ToggleDisplay Id BoxId -- TODO: make params Id BoxPath
  | Unbox BoxId BoxId -- TODO: make params BoxId BoxPath
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
  | Icon Icon.Msg


type NavMsg
  = Fullscreen
  | Back
