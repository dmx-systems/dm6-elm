module Size exposing (auto)

import ExtensionDef exposing (AutoSize)
import Model exposing (Model)



-- TODO: pass Env instead AutoSize -> Model?
auto : AutoSize -> Model -> Model
auto autoSize model =
  model
    |> autoSize [ model.boxId ]
    |> Tuple.second
