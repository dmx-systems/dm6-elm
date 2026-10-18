module Outcome exposing (Outcome, Directives, Storage(..), Undo(..), default, withDir, from,
  fromDir, map, mergeCmd, perform, command)

import Feature.Crdt as Crdt
import Model exposing (Model, Msg)
import Storage as S
import Undo exposing (UndoModel)

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


--

map : (Model -> Model) -> Outcome -> Outcome
map transform ({model} as out) =
  { out | model = transform model }


mergeCmd : Cmd Msg -> Outcome -> Outcome
mergeCmd cmd out =
  { out | cmd = Cmd.batch [out.cmd, cmd] }


perform : UndoModel -> Outcome -> (UndoModel, Cmd Msg)
perform undoModel {model, cmd, directives} =
  let
    performUndo : UndoModel -> UndoModel -- TODO: utilize CRDT for Undo, drop UndoModel
    performUndo undoModel_ =
      ( case directives.undo of
          NoUndo -> model
          SingleEdit -> Crdt.markSingleEdit undoModel_.present model
          EditStart -> Crdt.markEditStart undoModel_.present model
          EditEnd -> Crdt.markEditEnd model
          Undo -> Crdt.undo model
          Redo -> Crdt.redo model
      )
      |> Undo.swap undoModel_
    --
    performStorage : UndoModel -> (UndoModel, Cmd Msg)
    performStorage undoModel_ =
      case directives.storage of
        Store ->
          let
            (model_, cmd_) = S.store undoModel_.present
          in
          ( Undo.swap undoModel_ model_
          , Cmd.batch [ cmd, cmd_ ]
          )
        NoStore -> (undoModel_, cmd)
  in
  undoModel
    |> performUndo
    |> performStorage


--

command : msg -> Cmd msg
command msg =
  Task.succeed ()
    |> Task.perform (always msg)
