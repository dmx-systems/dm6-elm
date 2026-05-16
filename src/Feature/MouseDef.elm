module Feature.MouseDef exposing (..)

import ModelBase exposing (..)



type alias Model =
  { dragState : DragState
  , hover : Maybe Target -- The hovered topic.
                         -- Synthesized also on pointerdown when pointertype is "touch".
  }


init : Model
init =
  { dragState = NoDrag
  , hover = Nothing
  }


type DragState
  = DragInProgress TopicId BoxPath -- entered by DragStart message
  | NoDrag                         -- entered by Up message


type Msg
  -- Topic dragging
  = DragStart TopicId BoxPath (Point, PointerType) -- pointerdown on topic (Events.draggable)
  | Move (Point, PointerType)    -- (fired by handlers created by Events.globalMouseHandler)
  | Up                           -- (fired by handlers created by Events.globalMouseHandler)
  -- UI cancellation
  | Cancel -- pointerdown somewhere (fired by handlers created by Events.globalMouseHandler)
