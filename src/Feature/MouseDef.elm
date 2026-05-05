module Feature.MouseDef exposing (..)

import ModelBase exposing (..)



type alias Model =
  { dragState : DragState }


init : Model
init =
  { dragState = NoDrag }


type DragState
  = DragInProgress BoxId
  | NoDrag


type Msg
  -- Topic dragging
  = DragStart TopicId BoxPath (Point, PointerType) -- mouse down on topic
  | Drag (Point, PointerType)
  | DragStop
  -- UI cancellation
  | Cancel -- mouse down somewhere
