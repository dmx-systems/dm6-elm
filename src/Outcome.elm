module Outcome exposing (Outcome, Directives, Storage(..), History(..), default, withDir, from,
  fromDir, map, perform, command)

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
  , history : History
  }


type Storage
  = Store
  | NoStore -- default


type History
  = Push
  | Swap -- default
  | Reset
  --
  | Undo
  | Redo


--

default : Model -> Outcome
default model =
  Outcome model Cmd.none (Directives NoStore Swap)


withDir : Directives -> Model -> Outcome
withDir directives model =
  Outcome model Cmd.none directives


from : (Model, Cmd Msg) -> Outcome
from (model, cmd) =
  Outcome model cmd (Directives NoStore Swap)


fromDir : Directives -> (Model, Cmd Msg) -> Outcome
fromDir directives (model, cmd) =
  Outcome model cmd directives


--

map : (Model -> Model) -> Outcome -> Outcome
map transform ({model} as out) =
  { out | model = transform model }


perform : UndoModel -> Outcome -> (UndoModel, Cmd Msg)
perform undoModel {directives, cmd, model} =
  let
    performHistory : UndoModel -> UndoModel
    performHistory undoModel_ =
      case directives.history of
        Push -> Undo.push undoModel_ model
        Swap -> Undo.swap undoModel_ model
        Reset -> Undo.reset model
        Undo -> Undo.undo undoModel_
        Redo -> Undo.redo undoModel_
    --
    performStorage : UndoModel -> (UndoModel, Cmd Msg)
    performStorage undoModel_ =
      case directives.storage of
        Store ->
          ( undoModel_
          , Cmd.batch
              [ cmd
              , S.store undoModel_.present
              ]
          )
        NoStore -> (undoModel_, cmd)
  in
  undoModel
    |> performHistory
    |> performStorage


--

command : msg -> Cmd msg
command msg =
  Task.succeed ()
    |> Task.perform (\_ -> msg)
