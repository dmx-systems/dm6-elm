module Feature.Id exposing (get, update)

import Config as C
import Console
import Env exposing (Env)
import Feature.IdDef as IdDef
import Model exposing (Model, Msg(..))
import ModelBase exposing (..)
import Outcome exposing (Outcome)

import Random



get : Model -> (Id, Model)
get model =
  case model.id.pool of
    id_ :: ids -> (id_, { model | id = { pool = ids }})
    [] ->
      let
        _ = Console.logError "Id.get" "ID pool is empty" model.id.pool
      in
      ("### ERROR", model) -- ### TODO


requestIds : Model -> Cmd Msg
requestIds model =
  let
    length = model.id.pool |> List.length
  in
  if length < C.idPoolThreshold then
    idGenerator (C.idPoolSize - length)
      |> Random.generate (Id << IdDef.GotIds)
  else
    Cmd.none


-- TODO: use UUID generator
idGenerator : Int -> Random.Generator (List Id)
idGenerator count =
  Random.int 0 Random.maxInt
    |> Random.map String.fromInt
    |> Random.list count



-- UPDATE


update : IdDef.Msg -> Env -> Outcome
update msg ({model} as env) =
  case msg of
    IdDef.GotIds ids ->
      model
        |> fillPool ids
        |> Outcome.default -- ### TODO?


fillPool : List Id -> Model -> Model
fillPool ids ({id} as model) =
  { model | id = { id | pool = id.pool ++ ids }}
