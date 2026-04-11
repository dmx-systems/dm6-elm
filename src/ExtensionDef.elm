module ExtensionDef exposing (..)

import Model exposing (Model, Msg)
import ModelBase exposing (Id, BoxId, BoxPath, Target, Point, Rectangle)

import Html exposing (Html)



-- TYPES


-- "view"

{-| A box renderer basically takes a boxId and returns HTML.

Note: copy in TopicMap.View.elm to avoid a cyclic dependency.
Can *not* be defined in ModelBase as it needs Model.
Can also not be defined in BoxRendererDef as Model imports BoxRendererDef.
-> Move it to Model?
-}
type alias BoxRenderer =
  BoxId -> BoxPath -> Model -> Html Msg


{-| A box renderer transformation that enables the dispatching box renderer to inject itself
into the actual box renderer (e.g. TopicMap, List). ### FIXDOC
The actual renderers "view" functions are of this type.
Note: the actual box renderers get access to the dispatching box renderer as an argument of
their "view" function instead of importing a module. This avoids circular dependencies in
conjunction with recursively nested renderers.
-}
type alias NestingBoxRenderer =
  BoxId -> BoxPath -> BoxRenderer -> Model -> Html Msg


-- "hitTest"

type alias HitTest =
  BoxId -> BoxPath -> Point -> Maybe Id -> Model -> Maybe Target


type alias NestingHitTest =
  BoxId -> BoxPath -> Point -> Maybe Id -> HitTest -> Model -> Maybe Target


-- "autoSize"

type alias AutoSize =
  BoxPath -> Model -> (Rectangle, Model)


type alias NestingAutoSize =
  BoxPath -> AutoSize -> Model -> (Rectangle, Model)
