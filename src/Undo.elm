module Undo exposing (UndoModel, undo, redo, push, swap, reset, hasPast, hasFuture)

import Model exposing (Model, Msg)
import Storage as S

import UndoList exposing (UndoList)



-- DEFINITION


type alias UndoModel = UndoList Model



-- BASIC OPERATIONS


undo : UndoModel -> (UndoModel, Cmd Msg)
undo undoModel =
  let
    newUndoModel = UndoList.undo undoModel
    newModel = Model.initTransient newUndoModel.present
  in
  newModel
  |> S.store
  |> swap newUndoModel


redo : UndoModel -> (UndoModel, Cmd Msg)
redo undoModel =
  let
    newUndoModel = UndoList.redo undoModel
    newModel = Model.initTransient newUndoModel.present
  in
  newModel
  |> S.store
  |> swap newUndoModel



-- STACK MANAGEMENT


push : UndoModel -> (Model, Cmd Msg) -> (UndoModel, Cmd Msg)
push undoModel (model, cmd) =
  (UndoList.new model undoModel, cmd)


swap : UndoModel -> (Model, Cmd Msg) -> (UndoModel, Cmd Msg)
swap undoModel (model, cmd) =
  (UndoList.mapPresent (\_ -> model) undoModel, cmd)


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
