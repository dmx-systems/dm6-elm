module AppModel exposing (Model, Msg(..))

-- Core model & all the types you reference (Id, MapId, Point, Items, Maps, Map(..), Rectangle(..),
-- Selection, EditState(..), DisplayMode, EditMsg, NavMsg, MouseMsg, SearchMsg, IconMenuMsg, etc.)
-- Parameterized bus message; alias to avoid name collision with AppModel.Msg
-- These three are needed if you call Mouse.init / Search.init / IconMenu.init in this file
-- (use aliases so the `.init` calls resolve)

import Extensions as Ext
import IconMenu exposing (..)
import Model exposing (..)
import Mouse exposing (..)
import Search exposing (..)


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


type Msg
    = AddTopic
    | MoveTopicToMap Id MapId Point Id MapId Point -- start point, random point (for target)
    | SwitchDisplay DisplayMode
    | Edit EditMsg
    | Nav NavMsg
    | Hide
    | Delete
    | NoOp
    | Ext (Ext.Msg Msg)
      -- components
    | Mouse MouseMsg
    | Search SearchMsg
    | IconMenu IconMenuMsg
