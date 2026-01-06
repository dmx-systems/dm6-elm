port module Storage exposing (store, storeWith, storeCmd, importJSON, exportJSON)

import Model exposing (Model, Msg)

import Json.Encode as E



-- PORTS


port storeModel : E.Value -> Cmd msg
port importJSON : () -> Cmd msg
port exportJSON : () -> Cmd msg



-- API


-- Convenience (for update function)
store : Model -> (Model, Cmd Msg)
store model =
  ( model, model |> storeCmd )


-- Convenience (for update function)
storeWith : (Model, Cmd Msg) -> (Model, Cmd Msg)
storeWith (model, cmd) =
  ( model
  , Cmd.batch
      [ cmd
      , model |> storeCmd
      ]
  )


storeCmd : Model -> Cmd Msg
storeCmd model =
  Model.encode model |> storeModel
