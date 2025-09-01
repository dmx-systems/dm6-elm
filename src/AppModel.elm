module AppModel exposing (..)

-- components

import Dict
import IconMenu exposing (IconMenuModel, IconMenuMsg)
import Model exposing (..)
import Mouse exposing (MouseModel, MouseMsg)
import Search exposing (SearchModel, SearchMsg)


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
    , mouse : MouseModel
    , search : SearchModel
    , iconMenu : IconMenuModel
    }


default : Model
default =
    { items = Dict.empty
    , maps =
        Dict.singleton 0
        -- map 0 is the "home map", it has no corresponding topic
        <|
            Map 0 (Rectangle 0 0 0 0) Dict.empty
    , mapPath = [ 0 ]
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
