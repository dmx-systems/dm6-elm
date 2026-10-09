module Feature.Id exposing (new, requestIds, requestIdsWith, update, fillPool)

import Config as C
import Console
import Env exposing (Env)
import Feature.IdDef as IdDef
import Model exposing (Model, Msg(..))
import ModelBase exposing (..)
import Outcome exposing (Outcome)

import Random



new : Model -> (Id, Model)
new model =
  case model.id.pool of
    id :: ids -> (id, { model | id = { pool = ids }})
    [] ->
      let
        _ = Console.logError "Id.new" "ID pool is empty" model.id.pool
      in
      ("### ERROR", model) -- ### TODO


requestIds : Model -> Cmd Msg
requestIds =
  requestIdsWith (Id << IdDef.GotIds)


requestIdsWith : (List Id -> Msg) -> Model -> Cmd Msg
requestIdsWith toMsg model =
  let
    length = model.id.pool |> List.length
  in
  if length < C.idPoolThreshold then
    idGenerator (C.idPoolSize - length)
      |> Random.generate toMsg
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
