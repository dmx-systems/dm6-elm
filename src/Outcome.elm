module Outcome exposing (Outcome, Directives, Persistence(..), History(..), with, map, exec)

import Model exposing (Model, Msg)
import Storage as S
import Undo exposing (UndoModel)



type alias Outcome =
  { directives : Directives
  , cmd : Cmd Msg
  , model : Model
  }


type alias Directives =
  { persistence : Persistence
  , history : History
  }


type Persistence
  = Persistent
  | Transient


type History
  = StoreUndo
  | SkipUndo


--

with : Cmd Msg -> Model -> Outcome
with cmd model =
  Outcome (Directives Transient SkipUndo) cmd model


map : (Model -> Model) -> Outcome -> Outcome
map transform ({model} as out) =
  { out | model = transform model }


exec : UndoModel -> Outcome -> (UndoModel, Cmd Msg)
exec undoModel {directives, cmd, model} =
  let
    mct =
      case directives.persistence of
        Persistent -> S.storeWith (model, cmd)
        Transient -> (model, cmd)
  in
  case directives.history of
    StoreUndo -> Undo.push undoModel mct
    SkipUndo -> Undo.swap undoModel mct
