port module Storage exposing (store, storeWith, importJSON, exportJSON)

import Model exposing (Model, Msg)

import Json.Encode as E



-- PORTS


port storeModel : E.Value -> Cmd msg
port importJSON : () -> Cmd msg
port exportJSON : () -> Cmd msg


--

store : Model -> (Model, Cmd Msg)
store model =
  (model, Model.encode model |> storeModel)


storeWith : (Model, Cmd Msg) -> (Model, Cmd Msg)
storeWith (model, cmd) =
  ( model
  , Cmd.batch
    [ cmd
    , Model.encode model |> storeModel
    ]
  )
