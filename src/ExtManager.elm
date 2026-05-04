module ExtManager exposing (ext)

import Box
import Env exposing (ExtManager, Env2)
import Extension
import Model exposing (Model, Msg)
import ModelBase exposing (..)
-- box renderers
import TopicList.Geometry
import TopicList.TopicList
import TopicMap.Geometry
import TopicMap.Size
import TopicMap.View
import TopicMap.Mouse

import Dict exposing (Dict)
import Html exposing (Html, text)



-- TYPES


type alias Extension =
  { label : ExtLabel
  , view : NestingBoxRenderer
  , hitTest : NestingHitTest
  , autoSize : NestingAutoSize
  , toolbar : NestingToolbar
  , mouseMove : NestingMouseMove
  , mouseUp : NestingMouseUp
  }


{-| A box renderer transformation that enables the dispatching box renderer to inject itself
into the actual box renderer (e.g. TopicMap, List). ### FIXDOC
The actual renderers "view" functions are of this type.
Note: the actual box renderers get access to the dispatching box renderer as an argument of
their "view" function instead of importing a module. This avoids circular dependencies in
conjunction with recursively nested renderers.
-}
type alias NestingBoxRenderer =
  BoxId -> BoxPath -> Env2 -> Html Msg


type alias NestingHitTest =
  BoxId -> BoxPath -> Point -> Maybe TopicId -> Env2 -> Maybe Target


type alias NestingAutoSize =
  BoxPath -> Env2 -> (Rectangle, Model)


-- TODO: wording, note: ExtManager is not passed
type alias NestingToolbar =
  BoxId -> Model -> ToolbarPos


-- TODO: wording
type alias NestingMouseMove =
  Point -> Env2 -> (Model, Cmd Msg)


-- TODO: wording
type alias NestingMouseUp =
  Env2 -> (Model, Cmd Msg)



-- VALUES


ext : ExtManager
ext =
  { view = view
  , hitTest = hitTest
  , autoSize = autoSize
  , toolbar = toolbar
  , mouseMove = mouseMove
  , mouseUp = mouseUp
  , all = all
  }


-- key = renderer name (String)
-- Note: custom types can't be used as Dict keys
registry : Dict String Extension
registry =
  Dict.fromList
    [ ("TopicMap",
        { label = "Topic Map"
        , view = TopicMap.View.view
        , hitTest = TopicMap.Geometry.hitTest
        , autoSize = TopicMap.Size.autoSize
        , toolbar = TopicMap.Geometry.toolbarPos
        , mouseMove = TopicMap.Mouse.mouseMove
        , mouseUp = TopicMap.Mouse.mouseUp
        }
      )
    , ("TopicList",
        { label = "List"
        , view = TopicList.TopicList.view
        , hitTest = TopicList.Geometry.hitTest
        , autoSize = TopicList.Geometry.autoSize
        , toolbar = TopicList.Geometry.toolbarPos
        , mouseMove = TopicList.TopicList.mouseMove
        , mouseUp = TopicList.TopicList.mouseUp
        }
      )
    ]



--


{-| The dispatching box renderer.
Basically it takes a box ID and dispatches to the renderer (e.g. TopicMap, List) that is in
charge. By passing itself it enables a box renderer to call it for rendering nested boxes.
Note: structurally the dispatching box renderer *is* a box renderer: it takes a box ID and
returns HTML.
-}
view : BoxId -> BoxPath -> Model -> Html Msg
view boxId boxPath model =
  dispatch boxId model (text "Renderer ?")
    (\env renderer -> renderer.view boxId boxPath env)


hitTest : BoxId -> BoxPath -> Point -> Maybe TopicId -> Model -> Maybe Target
hitTest boxId boxPath pos excludeTopicId model =
  dispatch boxId model Nothing
    (\env renderer -> renderer.hitTest boxId boxPath pos excludeTopicId env)


autoSize : BoxPath -> Model -> (Rectangle, Model)
autoSize boxPath model =
  dispatch (Box.firstId boxPath) model (Rectangle 0 0 0 0, model)
    (\env renderer -> renderer.autoSize boxPath env)


toolbar : BoxId -> Model -> ToolbarPos
toolbar boxId model =
  dispatch boxId model (ToolbarPos (\_ -> Point 0 0) (\_ -> Point 0 0))
    (\env renderer -> renderer.toolbar boxId model)


mouseMove : BoxId -> Point -> Model -> (Model, Cmd Msg)
mouseMove boxId pos model =
  dispatch boxId model (model, Cmd.none)
    (\env renderer -> renderer.mouseMove pos env)


mouseUp : BoxId -> Model -> (Model, Cmd Msg)
mouseUp boxId model =
  dispatch boxId model (model, Cmd.none)
    (\env renderer -> renderer.mouseUp env)


dispatch : BoxId -> Model -> result -> (Env2 -> Extension -> result) -> result
dispatch boxId model errVal callExtWith =
  Box.rendererOf boxId model
    |> Maybe.andThen (\renderer -> registry |> Dict.get (Extension.toString renderer))
    |> Maybe.map (callExtWith <| Env2 model ext)
    |> Maybe.withDefault errVal


--

all : Extensions
all =
  registry
    |> Dict.toList
    |> List.map
      (\(name, {label}) -> (name, label))
