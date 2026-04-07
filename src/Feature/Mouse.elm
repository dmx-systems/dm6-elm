module Feature.Mouse exposing (..)

import ModelBase exposing (..)
import Time



type alias Model =
  { dragState : DragState
  , hover : Maybe (Id, BoxPath)
  }


init : Model
init =
  { dragState = NoDrag
  , hover = Nothing
  }


type DragState
  = WaitForStartTime Id BoxPath Point -- start point (mouse)
  | DragEngaged Time.Posix Id BoxPath Point -- start point (mouse)
  | WaitForEndTime Time.Posix Id BoxPath Point -- start point (mouse)
  | Drag DragMode Id BoxPath Point Point (Maybe (Id, BoxPath)) -- orig topic pos
                                                               -- last point (mouse)
  | NoDrag


type DragMode
  = DragTopic
  | DraftAssoc


type Msg
  = Down -- mouse down somewhere
  -- Topic
  | DownOnTopic Id BoxPath (Point, PointerType) -- mouse down on topic, drag engaged
  | Move (Point, PointerType)
  | Up
  | Time Time.Posix
  -- Association
  | AssocClicked Id BoxPath
