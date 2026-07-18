module Outcome exposing (Outcome, Directives, Storage(..), History(..), default, from, new,
  newWith, with, map, perform, command)

import Model exposing (Model, Msg)
import Storage as S
import Undo exposing (UndoModel)

import Task



type alias Outcome =
  { directives : Directives
  , cmd : Cmd Msg
  , model : Model
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


-- TODO: replace these 5 by Builder API?

default : Model -> Outcome
default model =
  Outcome (Directives NoStore Swap) Cmd.none model


from : Directives -> Model -> Outcome
from directives model =
  Outcome directives Cmd.none model


new : (Model, Cmd Msg) -> Outcome
new (model, cmd) =
  Outcome (Directives NoStore Swap) cmd model


newWith : Directives -> (Model, Cmd Msg) -> Outcome
newWith directives (model, cmd) =
  Outcome directives cmd model


with : Cmd Msg -> Model -> Outcome
with cmd model =
  Outcome (Directives NoStore Swap) cmd model


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
