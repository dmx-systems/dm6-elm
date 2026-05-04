module Feature.MouseDef exposing (..)

import ModelBase exposing (..)



type alias Model =
  { dragState : DragState }


init : Model
init =
  { dragState = NoDrag }


type DragState
  = DragEngaged BoxId
  | NoDrag


type Msg
  = Move (Point, PointerType)
  | Up
  | Cancel -- TODO: mouse down somewhere, cancelling, rename "Cancel"?
