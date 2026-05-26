module Undo exposing (UndoModel, undo, redo, push, swap, reset, hasPast, hasFuture)

import Model exposing (Model, Msg, resetTransient)
import Utils as U

import UndoList exposing (UndoList)



-- DEFINITION


type alias UndoModel = UndoList Model



-- BASIC OPERATIONS


undo : UndoModel -> UndoModel
undo undoModel =
  undoModel
    |> UndoList.undo


redo : UndoModel -> UndoModel
redo undoModel =
  undoModel
    |> UndoList.redo



-- STACK MANAGEMENT


{- Push the current present (as in UndoModel) to the past and make the given model (as in tupel)
the new present.
-}
push : UndoModel -> (Model, Cmd Msg) -> (UndoModel, Cmd Msg)
push undoModel (model, cmd) =
  let
    _ = U.info "Undo.push" "<------------------"
  in
  ( undoModel
      |> UndoList.mapPresent resetTransient
      |> UndoList.new model
  , cmd
  )



{- Swap the current present (as in UndoModel) with the given model (as in tupel).
-}
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
