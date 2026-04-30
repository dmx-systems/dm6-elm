module Feature.MouseDef exposing (..)

import ModelBase exposing (..)
import Time



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
  = WaitForStartTime TopicId BoxPath Point -- start point (mouse)
  | DragEngaged Time.Posix TopicId BoxPath Point -- start point (mouse)
  | WaitForEndTime Time.Posix TopicId BoxPath Point -- start point (mouse)
  | Drag DragMode TopicId BoxPath Point Point (Maybe Target) -- orig topic pos
                                                        -- last point (mouse)
  | NoDrag


type DragMode
  = DragTopic
  | DraftAssoc


type Msg
  = Down -- mouse down somewhere
  -- Topic
  | DownOnTopic TopicId BoxPath (Point, PointerType) -- mouse down on topic, drag engaged
  | Move (Point, PointerType)
  | Up
  | Time Time.Posix
