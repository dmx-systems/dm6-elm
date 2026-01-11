module Feature.Mouse exposing (..)

import ModelParts exposing (..)
import Time



type alias Model =
  { dragState : DragState }


init : Model
init =
  { dragState = NoDrag Nothing }


type DragState
  = WaitForStartTime Id BoxPath Point -- start point (mouse)
  | DragEngaged Time.Posix Id BoxPath Point -- start point (mouse)
  | WaitForEndTime Time.Posix Id BoxPath Point -- start point (mouse)
  | Drag DragMode Id BoxPath Point Point (Maybe (Id, BoxPath)) -- orig topic pos
                                                               -- last point (mouse)
  | NoDrag (Maybe (Id, BoxPath)) -- hover target


type DragMode
  = DragTopic
  | DraftAssoc


type Msg
  = Down -- mouse down somewhere
  -- Topic
  | DownOnTopic Id BoxPath Point -- mouse down on topic, drag engaged
  | Move Point
  | Up
  | Hover Id BoxPath
  | Unhover Id BoxPath
  | Time Time.Posix
  -- Association
  | AssocClicked Id BoxPath
