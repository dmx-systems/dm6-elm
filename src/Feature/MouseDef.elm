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
  = DragStarted TopicId BoxPath BoxPath Point
                                 -- entered by DownOnTopic message, start pointer position
                                 -- data: 2nd path is "interaction box path"
  | NoDrag -- entered by Up message


type Msg
  -- Topic dragging
  = DownOnTopic TopicId BoxPath BoxPath (Point, PointerType)
                                 -- pointerdown on topic (Events.draggable)
                                 -- data: 2nd path is "interaction box path"
  | Move (Point, PointerType)    -- (fired by handlers created by Events.globalMouseHandler)
  | Up                           -- (fired by handlers created by Events.globalMouseHandler)
  -- UI cancellation
  | Cancel -- pointerdown somewhere (fired by handlers created by Events.globalMouseHandler)
