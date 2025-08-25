module Mouse exposing (..)

import Model exposing (Class, Id, MapId, Point)
import Time



type alias MouseModel =
  { dragState : DragState }


init : MouseModel
init =
  { dragState = NoDrag }


type DragState
  = WaitForStartTime Class Id MapId Point -- start point (mouse)
  | DragEngaged Time.Posix Class Id MapId Point -- start point (mouse)
  | WaitForEndTime Time.Posix Class Id MapId Point -- start point (mouse)
  | Drag DragMode Id MapId Point Point (Maybe (Id, MapId)) -- orig topic pos, last point (mouse)
  | NoDrag


type DragMode
  = DragTopic
  | DrawAssoc


type MouseMsg
  = Down -- mouse down somewhere
  | DownItem Class Id MapId Point -- mouse down on an item where a drag can be engaged
  | Move Point
  | Up
  | Over Class Id MapId
  | Out Class Id MapId
  | Time Time.Posix
