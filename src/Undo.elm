module Undo exposing (UndoModel, undo, redo, push, swap, mapPresent, reset, hasPast, hasFuture)

import Model exposing (Model, Msg)
import Utils as U

import UndoList exposing (UndoList)



-- DEFINITION


type alias UndoModel = UndoList Model



-- BASIC OPERATIONS


undo : UndoModel -> UndoModel
undo undoModel =
  UndoList.undo undoModel
  -- TODO: reset some transient state, e.g. close search menu


redo : UndoModel -> UndoModel
redo undoModel =
  UndoList.redo undoModel



-- STACK MANAGEMENT


push : UndoModel -> (Model, Cmd Msg) -> (UndoModel, Cmd Msg)
push undoModel (model, cmd) =
  let
    _ = U.info "Undo.push" "<------------------"
  in
  (UndoList.new model undoModel, cmd)


swap : UndoModel -> (Model, Cmd Msg) -> (UndoModel, Cmd Msg)
swap undoModel (model, cmd) =
  (UndoList.mapPresent (\_ -> model) undoModel, cmd)


-- Not used
mapPresent : (Model -> Model) -> (UndoModel, Cmd Msg) -> (UndoModel, Cmd Msg)
mapPresent transform (undoModel, cmd) =
  (UndoList.mapPresent transform undoModel, cmd)


reset : (Model, Cmd Msg) -> (UndoModel, Cmd Msg)
reset (model, cmd) =
  (UndoList.fresh model, cmd)



-- QUERY


hasPast : UndoModel -> Bool
hasPast undoModel =
  undoModel
    |> UndoList.hasPast


hasFuture : UndoModel -> Bool
hasFuture undoModel =
  undoModel
    |> UndoList.hasFuture
