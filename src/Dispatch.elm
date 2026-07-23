module Dispatch exposing (dispatch)

import Box
import Console
import Env exposing (Env, Dispatch, Renderers, RendererLabel)
import Model exposing (Model, Msg)
import ModelBase exposing (..)
import Outcome exposing (Outcome)
import Renderer
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


type alias Renderer =
  { label : RendererLabel
  -- Hooks
  , init : InitHook
  , view : ViewHook
  , hitTest : HitTestHook
  , autoSize : AutoSizeHook
  , dragStart : DragStartHook
  , drag : DragHook
  , dragAccept : DragAcceptHook
  , dragStop : DragStopHook
  }


-- Renderer Hooks
--
-- Implemented by the **renderer developer**.
-- Called by the renderer dispatcher (see "dispatch_" function below).


type alias InitHook =
  BoxId -> Model -> Model


{-| A box renderer transformation that enables the dispatching box renderer to inject itself
into the actual box renderer (e.g. TopicMap, List). ### FIXDOC
The actual renderers "view" functions are of this type.
Note: the actual box renderers get access to the dispatching box renderer as an argument of
their "view" function instead of importing a module. This avoids circular dependencies in
conjunction with recursively nested renderers.
-}
type alias ViewHook =
  BoxId -> BoxPath -> Env -> Html Msg


-- Point is in box-local coordinates
type alias HitTestHook =
  BoxId -> BoxPath -> Point -> Maybe TopicId -> Env -> Maybe BoxTarget


type alias AutoSizeHook =
  BoxPath -> Env -> (Rectangle, Model)


-- Drag and Drop

-- Note: no drag specific parameters here. A renderer's "dragStart" handler operates on
-- (feature module) Mouse's "dragSource" directly.
type alias DragStartHook =
  Env -> (Model, Cmd Msg)


type alias DragHook =
  Point -> Env -> (Model, Cmd Msg)


type alias DragAcceptHook =
  Point -> Env -> (Model, Maybe Target)


type alias DragStopHook =
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
  , dragAccept = dragAccept
  , dragStop = dragStop
  -- List of all renderers
  , all = all
  }


-- key = renderer name (String)
-- Note: custom types can't be used as Dict keys
renderers : Dict String Renderer
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
        , dragAccept = TopicMap.Mouse.dragAccept
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
        , dragAccept = TopicList.Mouse.dragAccept
        , dragStop = TopicList.Mouse.dragStop
        }
      )
    ]



-- Implementation of the dispatch functions as specified in Env.elm


init : BoxId -> Model -> (BoxId -> Model -> Model)
init boxId model =
  dispatch_ boxId model (\_ _ -> model)
    (\env renderer -> renderer.init)


{-| The dispatching box renderer.
Basically it takes a box ID and dispatches to the renderer (e.g. TopicMap, TopicList) in charge.
By passing itself it enables a box renderer to call it for rendering nested boxes.
Note: structurally the dispatching box renderer *is* a box renderer: it takes a box ID and
returns HTML. ### FIXDOC
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


dragAccept : BoxId -> Point -> Model -> (Model, Maybe Target)
dragAccept boxId pos model =
  dispatch_ boxId model (model, Nothing)
    (\env renderer -> renderer.dragAccept pos env)


dragStop : BoxId -> Model -> Outcome
dragStop boxId model =
  dispatch_ boxId model (Outcome.default model)
    (\env renderer -> renderer.dragStop env)


dispatch_ : BoxId -> Model -> result -> (Env -> Renderer -> result) -> result
dispatch_ boxId model errVal callHook =
  Box.rendererOf boxId model
    |> Maybe.andThen lookup
    |> Maybe.map (callHook <| Env model dispatch)
    |> Maybe.withDefault errVal


--

all : Renderers
all =
  renderers
    |> Dict.toList
    |> List.map
      (\(name, {label}) -> (name, label))


lookup : Renderer.Renderer -> Maybe Renderer
lookup renderer =
  renderers
    |> Dict.get (Renderer.toString renderer)
