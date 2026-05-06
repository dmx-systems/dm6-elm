module Feature.MouseDef exposing (..)

import ModelBase exposing (..)



type alias Model =
  { dragState : DragState
  , hover : Maybe Target -- TODO: make it explicitly TopicTarget?
  }


init : Model
init =
  { dragState = NoDrag
  , hover = Nothing
  }


type DragState
  = NoDrag
  | DragInProgress TopicId BoxId


type Msg
  -- Topic dragging
  = DragStart TopicId BoxPath (Point, PointerType) -- mouse down on topic
  | Move (Point, PointerType)
  | Up
  -- UI cancellation
  | Cancel -- mouse down somewhere
