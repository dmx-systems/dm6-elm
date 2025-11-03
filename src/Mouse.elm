module Mouse exposing (..)

import Model exposing (Class, Id, MapPath, Point)
import Time



type alias Model =
  { dragState : DragState }


init : Model
init =
  { dragState = NoDrag }


type DragState
  = WaitForStartTime Class Id MapPath Point -- start point (mouse)
  | DragEngaged Time.Posix Class Id MapPath Point -- start point (mouse)
  | WaitForEndTime Time.Posix Class Id MapPath Point -- start point (mouse)
  | Drag DragMode Id MapPath Point Point (Maybe (Id, MapPath)) -- orig topic pos,
                                                               -- last point (mouse)
  | NoDrag


type DragMode
  = DragTopic
  | DraftAssoc


type Msg
  = Down -- mouse down somewhere
  | DownOnItem Class Id MapPath Point -- mouse down on an item where a drag can be engaged
  | Move Point
  | Up
  | Over Class Id MapPath
  | Out Class Id MapPath
  | Time Time.Posix
