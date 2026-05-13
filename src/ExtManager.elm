module ExtManager exposing (ext)

import Box
import Env exposing (ExtManager, Env2)
import Extension
import Model exposing (Model, Msg)
import ModelBase exposing (..)
-- box renderers
import TopicList.Geometry
import TopicList.BoxProps
import TopicMap.Geometry
import TopicMap.Mouse
import TopicMap.View
import TopicMap.BoxProps

import Dict exposing (Dict)
import Html exposing (Html, text)



-- TYPES


type alias Extension =
  { label : ExtLabel
  , init : ExtInit
  , view : NestingBoxRenderer
  , hitTest : NestingHitTest
  , autoSize : NestingAutoSize
  , toolbar : NestingToolbar
  , dragStart : NestingDragStart
  , drag : NestingDrag
  , dragStop : NestingDragStop
  , addTopic : AddTopic
  }


type alias ExtInit =
  BoxId -> Model -> Model


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
type alias NestingDragStart =
  TopicId -> BoxPath -> Point -> PointerType -> Env2 -> (Model, Cmd Msg)


-- TODO: wording
type alias NestingDrag =
  Point -> Env2 -> (Model, Cmd Msg)


-- TODO: wording
type alias NestingDragStop =
  Env2 -> (Model, Cmd Msg)


type alias AddTopic =
  TopicId -> BoxId -> PosHint -> Env2 -> (Model, Cmd Msg)



-- VALUES


ext : ExtManager
ext =
  { init = init
  , view = view
  , hitTest = hitTest
  , autoSize = autoSize
  , toolbar = toolbar
  , dragStart = dragStart
  , drag = drag
  , dragStop = dragStop
  , addTopic = addTopic
  , all = all
  }


-- key = renderer name (String)
-- Note: custom types can't be used as Dict keys
registry : Dict String Extension
registry =
  Dict.fromList
    [ ("TopicMap",
        { label = "Topic Map"
        , init = TopicMap.BoxProps.init
        , view = TopicMap.View.view
        , hitTest = TopicMap.Geometry.hitTest
        , autoSize = TopicMap.Geometry.autoSize
        , toolbar = TopicMap.Geometry.toolbarPos
        , dragStart = TopicMap.Mouse.dragStart
        , drag = TopicMap.Mouse.drag
        , dragStop = TopicMap.Mouse.dragStop
        , addTopic = TopicMap.BoxProps.addTopic
        }
      )
    , ("TopicList",
        { label = "List"
        , init = TopicList.BoxProps.init
        , view = TopicList.BoxProps.view
        , hitTest = TopicList.Geometry.hitTest
        , autoSize = TopicList.Geometry.autoSize
        , toolbar = TopicList.Geometry.toolbarPos
        , dragStart = TopicList.BoxProps.dragStart
        , drag = TopicList.BoxProps.drag
        , dragStop = TopicList.BoxProps.dragStop
        , addTopic = TopicList.BoxProps.addTopic
        }
      )
    ]



-- Note: these functions implement the "capability" type signatures defined in Env.elm


init : BoxId -> Model -> Model
init boxId model =
  dispatch boxId model model
    (\env renderer -> renderer.init boxId model)


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


dragStart : TopicId -> BoxPath -> Point -> PointerType -> Model -> (Model, Cmd Msg)
dragStart topicId boxPath pos pointerType model =
  dispatch (Box.firstId boxPath) model (model, Cmd.none)
    (\env renderer -> renderer.dragStart topicId boxPath pos pointerType env)


drag : BoxId -> Point -> Model -> (Model, Cmd Msg)
drag boxId pos model =
  dispatch boxId model (model, Cmd.none)
    (\env renderer -> renderer.drag pos env)


dragStop : BoxId -> Model -> (Model, Cmd Msg)
dragStop boxId model =
  dispatch boxId model (model, Cmd.none)
    (\env renderer -> renderer.dragStop env)


addTopic : TopicId -> BoxId -> PosHint -> Model -> (Model, Cmd Msg)
addTopic topicId boxId posHint model =
  dispatch boxId model (model, Cmd.none)
    (\env renderer -> renderer.addTopic topicId boxId posHint env)


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
