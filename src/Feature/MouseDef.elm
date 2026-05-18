module Feature.MouseDef exposing (..)

import ModelBase exposing (..)



type alias Model =
  { dragState : DragState
  -- The geometrically hovered topic. (Drop semantics is added by particular renderer e.g.
  -- TopicMap.Mouse.updateTarget.) Synthesized also on pointerdown for touch devices.
  , hover : Maybe Target
  }


init : Model
init =
  { dragState = NoDrag
  , hover = Nothing
  }


type DragState
  = DragInProgress TopicId BoxPath Point -- entered by DragStart message, start pointer position
                                         -- TODO: rename to DragStarted?
  | NoDrag                               -- entered by Up message


type Msg
  -- Topic dragging
  = DragStart TopicId BoxPath (Point, PointerType) -- pointerdown on topic (Events.draggable)
                                                   -- TODO: rename to DownOnTopic?
  | Move (Point, PointerType)    -- (fired by handlers created by Events.globalMouseHandler)
  | Up                           -- (fired by handlers created by Events.globalMouseHandler)
  -- UI cancellation
  | Cancel -- pointerdown somewhere (fired by handlers created by Events.globalMouseHandler)
