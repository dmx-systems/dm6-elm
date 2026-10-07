port module Storage exposing (store, importJSON, exportJSON)

import Model exposing (Model, Msg)
import Feature.Sync as Sync

import Json.Encode as E



-- PORTS


port storeModel : (E.Value, E.Value) -> Cmd msg
port importJSON : () -> Cmd msg
port exportJSON : () -> Cmd msg



-- API


store : Model -> (Model, Cmd Msg)
store model =
  let
    (model_, value) = Sync.encodeDoc model
  in
  ( model_
  , storeModel
      ( model_ |> Model.encode
      , value
      )
  )
