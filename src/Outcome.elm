module Outcome exposing (Outcome, Directives, Storage(..), Undo(..), default, withDir, from,
  fromDir, with, map, mergeCmd, perform, performWith, command)

import Feature.Crdt as Crdt
import Model exposing (Model, Msg)
import Storage as S

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
  | NoStore -- default


type Undo
  = NoUndo
  | SingleEdit
  | EditStart
  | EditEnd
  --
  | Undo
  | Redo


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


perform : Outcome -> (Model, Cmd Msg)
perform outcome =
  performWith outcome.model outcome


performWith : Model -> Outcome -> (Model, Cmd Msg)
performWith oldModel {model, cmd, directives} =
  let
    performUndo : Model
    performUndo =
      case directives.undo of
        NoUndo -> model
        SingleEdit -> Crdt.markSingleEdit oldModel model
        EditStart -> Crdt.markEditStart oldModel model
        EditEnd -> Crdt.markEditEnd model
        Undo -> Crdt.undo model
        Redo -> Crdt.redo model
    --
    performStorage : Model -> (Model, Cmd Msg)
    performStorage model_ =
      case directives.storage of
        Store -> S.store model_ |> Model.mergeCmd cmd
        NoStore -> (model_, cmd)
  in
  performUndo
    |> performStorage


-- TODO: move to Model
command : msg -> Cmd msg
command msg =
  Task.succeed ()
    |> Task.perform (always msg)
