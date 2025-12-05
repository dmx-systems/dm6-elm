module Feature.Mouse exposing (..)

import ModelParts exposing (..)
import Time



type alias Model =
  { dragState : DragState }


init : Model
init =
  { dragState = NoDrag Nothing }


type DragState
  = WaitForStartTime Class Id BoxPath Point -- start point (mouse)
  | DragEngaged Time.Posix Class Id BoxPath Point -- start point (mouse)
  | WaitForEndTime Time.Posix Class Id BoxPath Point -- start point (mouse)
  | Drag DragMode Id BoxPath Point Point (Maybe (Id, BoxPath)) -- orig topic pos
                                                               -- last point (mouse)
  | NoDrag (Maybe (Id, BoxPath)) -- hover target


type DragMode
  = DragTopic
  | DraftAssoc


type Msg
  = Down -- mouse down somewhere
  | DownOnItem Class Id BoxPath Point -- mouse down on an item where a drag can be engaged
  | Move Point
  | Up
  | Hover Class Id BoxPath
  | Unhover Class Id BoxPath
  | Time Time.Posix
