module Outcome exposing (Outcome, Directives, Storage(..), History(..), default, from, new,
  newWith, with, map, exec)

import Model exposing (Model, Msg)
import Storage as S
import Undo exposing (UndoModel)



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


map : (Model -> Model) -> Outcome -> Outcome
map transform ({model} as out) =
  { out | model = transform model }


exec : UndoModel -> Outcome -> (UndoModel, Cmd Msg)
exec undoModel {directives, cmd, model} =
  let
    mct =
      case directives.storage of
        Store -> S.storeWith (model, cmd)
        NoStore -> (model, cmd)
  in
  case directives.history of
    Push -> Undo.push undoModel mct
    Swap -> Undo.swap undoModel mct
    Reset -> Undo.reset mct
