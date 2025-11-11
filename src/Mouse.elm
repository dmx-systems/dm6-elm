module Mouse exposing (..)

import Model exposing (Class, Id, BoxPath, Point)
import Time



type alias Model =
  { dragState : DragState }


init : Model
init =
  { dragState = NoDrag }


type DragState
  = WaitForStartTime Class Id BoxPath Point -- start point (mouse)
  | DragEngaged Time.Posix Class Id BoxPath Point -- start point (mouse)
  | WaitForEndTime Time.Posix Class Id BoxPath Point -- start point (mouse)
  | Drag DragMode Id BoxPath Point Point (Maybe (Id, BoxPath)) -- orig topic pos,
                                                               -- last point (mouse)
  | NoDrag


type DragMode
  = DragTopic
  | DraftAssoc


type Msg
  = Down -- mouse down somewhere
  | DownOnItem Class Id BoxPath Point -- mouse down on an item where a drag can be engaged
  | Move Point
  | Up
  | Over Class Id BoxPath
  | Out Class Id BoxPath
  | Time Time.Posix
