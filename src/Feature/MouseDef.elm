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


resetTransient : Model -> Model
resetTransient _ =
  init


type DragState
  = DragStarted TopicId BoxPath BoxPath Point
                                 -- entered by DownOnTopic message
                                 -- represents what is dragged, does not change while dragging
                                 -- data: topicId - the topic being dragged
                                 --       box path - direct topic context
                                 --       interaction box path - wider topic context
                                 --       start pos - where mousedown took place (client coord)
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
