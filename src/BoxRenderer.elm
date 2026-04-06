module BoxRenderer exposing (..)

import Model exposing (Model, Msg)
import ModelParts exposing (BoxId, BoxPath)

import Html exposing (Html)



-- TYPES


{-| A box renderer basically takes a boxId and returns HTML.

Note: copy in TopicMap.View.elm to avoid a cyclic dependency.
Can *not* be defined in ModelParts as it needs Model.
Can also not be defined in BoxRendererDef as Model imports BoxRendererDef.
-> Move it to Model?
-}
type alias BoxRenderer =
  BoxId -> BoxPath -> Model -> Html Msg


{-| A box renderer transformation that enables the dispatching box renderer to inject iteself
into the actual box renderer (e.g. TopicMap, List).
The actual renderers "view" functions are of this type.
Note: the actual box renderers get access to the dispatching box renderer as an argument of
their "view" function instead of importing a module. This avoids circular dependencies in
conjunction with recursively nested renderers.
-}
type alias BoxView =
  BoxRenderer -> BoxRenderer
