port module Storage exposing (store, importJSON, exportJSON)

import Feature.Crdt as Crdt
import Model exposing (Model, Msg)

import Json.Encode as E



-- PORTS


port storeModel : (E.Value, E.Value) -> Cmd msg
port importJSON : () -> Cmd msg
port exportJSON : () -> Cmd msg



-- API


store : Model -> (Model, Cmd Msg)
store model =
  let
    (model_, value) = Crdt.encodeDoc model
  in
  ( model_
  , storeModel
      ( model_ |> Model.encode
      , value
      )
  )
