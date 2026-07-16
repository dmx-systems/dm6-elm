port module Storage exposing (store, importJSON, exportJSON)

import Model exposing (Model, Msg)

import Json.Encode as E



-- PORTS


port storeModel : E.Value -> Cmd msg
port importJSON : () -> Cmd msg
port exportJSON : () -> Cmd msg



-- API


store : Model -> Cmd Msg
store model =
  model
    |> Model.encode
    |> storeModel
