module Outcome exposing (Outcome, Directives, Storage(..), Undo(..), default, withDir, from,
  fromDir, with, map, mergeCmd, command)

import Model exposing (Model, Msg)

import Task



type alias Outcome =
  { model : Model
  , cmd : Cmd Msg
  , directives : Directives
  }


type alias Directives =
  { storage : Storage
  , undo : Undo
  }


type Storage
  = Store
  | StoreOnly
  | NoStore -- default


type Undo
  = MarkSingle
  | MarkBegin
  | MarkEnd
  | NoUndo -- default


--

default : Model -> Outcome
default model =
  Outcome model Cmd.none (Directives NoStore NoUndo)


withDir : Directives -> Model -> Outcome
withDir directives model =
  Outcome model Cmd.none directives


from : (Model, Cmd Msg) -> Outcome
from (model, cmd) =
  Outcome model cmd (Directives NoStore NoUndo)


fromDir : Directives -> (Model, Cmd Msg) -> Outcome
fromDir directives (model, cmd) =
  Outcome model cmd directives


with : (Model -> Cmd Msg) -> Directives -> Model -> Outcome
with toCmd directives model =
  Outcome model (toCmd model) directives


--

map : (Model -> Model) -> Outcome -> Outcome
map transform ({model} as out) =
  { out | model = transform model }


mergeCmd : Cmd Msg -> Outcome -> Outcome
mergeCmd cmd out =
  { out | cmd = Cmd.batch [out.cmd, cmd] }


-- TODO: move to Model?
command : msg -> Cmd msg
command msg =
  Task.succeed ()
    |> Task.perform (always msg)
