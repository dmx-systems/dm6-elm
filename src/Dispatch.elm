module Dispatch exposing (dispatch)

import Box
import Console
import Env exposing (Env, Dispatch, Extensions, ExtLabel)
import Model exposing (Model, Msg)
import ModelBase exposing (..)
import Outcome exposing (Outcome)
import RendererDef
-- box renderers
import TopicList.Mouse
import TopicList.TopicList
import TopicList.View
import TopicMap.TopicMap
import TopicMap.Geometry
import TopicMap.Mouse
import TopicMap.View

import Dict exposing (Dict)
import Html exposing (Html, text)



-- TYPES


type alias Extension =
  { label : ExtLabel
  -- Renderer Hooks
  , init : ExtInit
  , view : ExtBoxView
  , hitTest : ExtHitTest
  , autoSize : ExtAutoSize
  , dragStart : ExtDragStart
  , drag : ExtDrag
  , updateDropTarget : ExtDropTargeting
  , dragStop : ExtDragStop
  }


-- Renderer Hooks
--
-- Implemented by the **renderer developer**.
-- Called by the renderer dispatcher (see "dispatch_" function below).
-- Compare to Env.elm


type alias ExtInit =
  BoxId -> Model -> Model


{-| A box renderer transformation that enables the dispatching box renderer to inject itself
into the actual box renderer (e.g. TopicMap, List). ### FIXDOC
The actual renderers "view" functions are of this type.
Note: the actual box renderers get access to the dispatching box renderer as an argument of
their "view" function instead of importing a module. This avoids circular dependencies in
conjunction with recursively nested renderers.
-}
type alias ExtBoxView =
  BoxId -> BoxPath -> Env -> Html Msg


-- Point is in box-local coordinates
type alias ExtHitTest =
  BoxId -> BoxPath -> Point -> Maybe TopicId -> Env -> Maybe BoxTarget


type alias ExtAutoSize =
  BoxPath -> Env -> (Rectangle, Model)


-- Drag and Drop

-- Note: no drag specific parameters here. An extension's "dragStart" handler operates on
-- (feature module) Mouse's "dragSource" directly.
type alias ExtDragStart =
  Env -> (Model, Cmd Msg)


type alias ExtDrag =
  Point -> Env -> (Model, Cmd Msg)


type alias ExtDropTargeting =
  Point -> Env -> (Model, Maybe Target)


type alias ExtDragStop =
  Env -> Outcome



-- VALUES


dispatch : Dispatch
dispatch =
  -- Renderer Hooks
  { init = init
  , view = view
  , hitTest = hitTest
  , autoSize = autoSize
  , dragStart = dragStart
  , drag = drag
  , updateDropTarget = updateDropTarget
  , dragStop = dragStop
  -- Query all renderers
  , all = all
  }


-- key = renderer name (String)
-- Note: custom types can't be used as Dict keys
renderers : Dict String Extension
renderers =
  Dict.fromList
    [ ("TopicMap",
        { label = "Topic Map"
        , init = TopicMap.TopicMap.init
        , view = TopicMap.View.view
        , hitTest = TopicMap.Geometry.hitTest
        , autoSize = TopicMap.Geometry.autoSize
        , dragStart = TopicMap.Mouse.dragStart
        , drag = TopicMap.Mouse.drag
        , updateDropTarget = TopicMap.Mouse.updateDropTarget
        , dragStop = TopicMap.Mouse.dragStop
        }
      )
    , ("TopicList",
        { label = "List"
        , init = TopicList.TopicList.init
        , view = TopicList.View.view
        , hitTest = TopicList.TopicList.hitTest
        , autoSize = TopicList.TopicList.autoSize
        , dragStart = TopicList.Mouse.dragStart
        , drag = TopicList.Mouse.drag
        , updateDropTarget = TopicList.Mouse.updateDropTarget
        , dragStop = TopicList.Mouse.dragStop
        }
      )
    ]



-- Note: these functions implement the Extension Point type signatures defined in Env.elm


init : BoxId -> Model -> (BoxId -> Model -> Model)
init boxId model =
  dispatch_ boxId model (\_ _ -> model)
    (\env renderer -> renderer.init)


{-| The dispatching box renderer.
Basically it takes a box ID and dispatches to the renderer (e.g. TopicMap, List) that is in
charge. By passing itself it enables a box renderer to call it for rendering nested boxes.
Note: structurally the dispatching box renderer *is* a box renderer: it takes a box ID and
returns HTML.
-}
view : BoxId -> BoxPath -> Model -> Html Msg
view boxId boxPath model =
  dispatch_ boxId model (text "Renderer ?")
    (\env renderer -> renderer.view boxId boxPath env)


hitTest : BoxId -> BoxPath -> Point -> Maybe TopicId -> Model -> Maybe BoxTarget
hitTest boxId boxPath pos maybeFilter model =
  dispatch_ boxId model Nothing
    (\env renderer -> renderer.hitTest boxId boxPath pos maybeFilter env)


autoSize : BoxPath -> Model -> (Rectangle, Model)
autoSize boxPath model =
  dispatch_ (Box.firstId boxPath) model (Rectangle 0 0 0 0, model)
    (\env renderer -> renderer.autoSize boxPath env)


-- Note: no drag specific parameters here. The dispatcher operates on Mouse's "dragSource"
-- directly. Important: the renderer is selected based on "interaction box path", not
-- "box path".
dragStart : Model -> (Model, Cmd Msg)
dragStart model =
  case model.mouse.dragSource of
    Just {ixBoxPath} ->
      dispatch_ (Box.firstId ixBoxPath) model (model, Cmd.none)
        (\env renderer -> renderer.dragStart env)
    _ ->
      let
        _ = Console.logError "Dispatch.dragStart" "Unexpected drag state" model.mouse.dragSource
      in
      (model, Cmd.none)


drag : BoxId -> Point -> Model -> (Model, Cmd Msg)
drag boxId pos model =
  dispatch_ boxId model (model, Cmd.none)
    (\env renderer -> renderer.drag pos env)


updateDropTarget : BoxId -> Point -> Model -> (Model, Maybe Target)
updateDropTarget boxId pos model =
  dispatch_ boxId model (model, Nothing)
    (\env renderer -> renderer.updateDropTarget pos env)


dragStop : BoxId -> Model -> Outcome
dragStop boxId model =
  dispatch_ boxId model (Outcome.default model)
    (\env renderer -> renderer.dragStop env)


dispatch_ : BoxId -> Model -> result -> (Env -> Extension -> result) -> result
dispatch_ boxId model errVal callExtWith =
  Box.rendererOf boxId model
    |> Maybe.andThen (\renderer -> renderers |> Dict.get (RendererDef.toString renderer))
    |> Maybe.map (callExtWith <| Env model dispatch)
    |> Maybe.withDefault errVal


--

all : Extensions
all =
  renderers
    |> Dict.toList
    |> List.map
      (\(name, {label}) -> (name, label))
