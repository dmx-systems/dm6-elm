module ExtManager exposing (ext)

import Box
import Env exposing (ExtManager, Env2)
import Extension
import Model exposing (Model, Msg)
import ModelBase exposing (..)
import Outcome exposing (Outcome)
-- box renderers
import TopicList.Model
import TopicList.Mouse
import TopicList.View
import TopicMap.BoxProps
import TopicMap.Geometry
import TopicMap.Mouse
import TopicMap.View
import Utils as U

import Dict exposing (Dict)
import Html exposing (Html, text)



-- TYPES


type alias Extension =
  { label : ExtLabel
  , init : ExtInit
  , view : NestingBoxRenderer
  , hitTest : NestingHitTest
  , autoSize : NestingAutoSize
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


-- Point is in box-local coordinates
type alias NestingHitTest =
  BoxId -> BoxPath -> Point -> Maybe TopicId -> Env2 -> Maybe Target


type alias NestingAutoSize =
  BoxPath -> Env2 -> (Rectangle, Model)


-- Note: no drag specific parameters here. An extension's "dragStart" handler operates on
-- (feature module) Mouse's "dragState" directly.
-- TODO: rename OnDragStart
type alias NestingDragStart =
  Env2 -> (Model, Cmd Msg)


-- TODO: wording
type alias NestingDrag =
  Point -> Env2 -> (Model, Cmd Msg)


-- TODO: wording
type alias NestingDragStop =
  Env2 -> Outcome


type alias AddTopic =
  TopicId -> BoxId -> PosHint -> Env2 -> (Model, Cmd Msg)



-- VALUES


ext : ExtManager
ext =
  { init = init
  , view = view
  , hitTest = hitTest
  , autoSize = autoSize
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
        , dragStart = TopicMap.Mouse.dragStart
        , drag = TopicMap.Mouse.drag
        , dragStop = TopicMap.Mouse.dragStop
        , addTopic = TopicMap.BoxProps.addTopic
        }
      )
    , ("TopicList",
        { label = "List"
        , init = TopicList.Model.init
        , view = TopicList.View.view
        , hitTest = TopicList.Model.hitTest
        , autoSize = TopicList.Model.autoSize
        , dragStart = TopicList.Mouse.dragStart
        , drag = TopicList.Mouse.drag
        , dragStop = TopicList.Mouse.dragStop
        , addTopic = TopicList.Model.addTopic
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


-- Note: no drag specific parameters here. The dispatcher operates on Mouse's "dragState"
-- directly. Important: the renderer is selected based on "interaction box path", not
-- "box path".
dragStart : Model -> (Model, Cmd Msg)
dragStart model =
  case model.mouse.dragState of
    Just {ixBoxPath} ->
      dispatch (Box.firstId ixBoxPath) model (model, Cmd.none)
        (\env renderer -> renderer.dragStart env)
    _ ->
      let
        _ = U.logError "ExtManager.dragStart" "Unexpected drag state" model.mouse.dragState
      in
      (model, Cmd.none)


drag : BoxId -> Point -> Model -> (Model, Cmd Msg)
drag boxId pos model =
  dispatch boxId model (model, Cmd.none)
    (\env renderer -> renderer.drag pos env)


dragStop : BoxId -> Model -> Outcome
dragStop boxId model =
  dispatch boxId model (Outcome.with Cmd.none model)
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
