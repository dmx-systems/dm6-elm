module Outcome exposing (Outcome, Directives, Storage(..), History(..), Undo(..), default,
  withDir, from, fromDir, map, mergeCmd, perform, command)

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
  , history : History -- TODO: drop
  , undo : Undo
  }


type Storage
  = Store
  | NoStore -- default


type History
  = Push
  | Swap -- default
  | Reset


type Undo
  = None
  | Single
  --
  | Undo
  | Redo


--

default : Model -> Outcome
default model =
  Outcome model Cmd.none (Directives NoStore Swap None)


withDir : Directives -> Model -> Outcome
withDir directives model =
  Outcome model Cmd.none directives


from : (Model, Cmd Msg) -> Outcome
from (model, cmd) =
  Outcome model cmd (Directives NoStore Swap None)


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
    performUndo : UndoModel -> UndoModel -- TODO: utilze CRDT for Undo, drop UndoModel
    performUndo undoModel_ =
      ( case directives.undo of
          None -> model
          Single -> Crdt.recordSingle undoModel_.present model
          Undo -> Crdt.undo model
          Redo -> Crdt.redo model
      )
      |> Undo.swap undoModel_
      {- case directives.history of -- TODO: drop
        Push -> Undo.push undoModel_ model
        Swap -> Undo.swap undoModel_ model
        Reset -> Undo.reset model
        Undo -> Undo.undo undoModel_
        Redo -> Undo.redo undoModel_
      -}
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
