module Env exposing (Env, Env2, ExtManager, autoSize, autoSize2, withModel, withModel2)

import Model exposing (Model, Msg)
import ModelBase exposing (..)
import Outcome exposing (Outcome)
import Undo exposing (UndoModel)

import Html exposing (Html)



-- TYPES


{-| The environment the application passes to the modules.
TODO: rename AppEnv?
-}
type alias Env =
  { model : Model
  , undoModel : UndoModel
  , ext : ExtManager
  }


{-| The environment the dispatcher passes to the extensions/feature modules
TODO: rename ModEnv (or just Env)?
-}
type alias Env2 =
  { model : Model
  , ext : ExtManager
  }


type alias ExtManager =
  { init : Init
  , view : BoxRenderer
  , hitTest : HitTest
  , autoSize : AutoSize
  , toolbar : Toolbar
  , dragStart : DragStart
  , drag : Drag
  , dragStop : DragStop
  , addTopic : AddTopic
  , all : Extensions
  }


-- Extension capabilities

type alias Init =
  BoxId -> Model -> Model


type alias BoxRenderer =
  BoxId -> BoxPath -> Model -> Html Msg


type alias HitTest =
  BoxId -> BoxPath -> Point -> Maybe TopicId -> Model -> Maybe Target


type alias AutoSize =
  BoxPath -> Model -> (Rectangle, Model)


type alias Toolbar =
  BoxPath -> Model -> ToolbarPos


-- Note: no drag specific parameters here. An extension's "dragStart" handler operates on
-- (feature module) Mouse's "dragState" directly.
type alias DragStart =
  Model -> (Model, Cmd Msg)


type alias Drag =
  BoxId -> Point -> Model -> (Model, Cmd Msg)


type alias DragStop =
  BoxId -> Model -> Outcome


type alias AddTopic =
  TopicId -> BoxId -> PosHint -> Model -> (Model, Cmd Msg)



-- HELPER


-- TODO: consolidate

autoSize : Env -> Model -> Model
autoSize {ext} model =
  model
    |> ext.autoSize [ model.boxId ]
    |> Tuple.second


autoSize2 : Env2 -> Model -> Model
autoSize2 {ext} model =
  model
    |> ext.autoSize [ model.boxId ]
    |> Tuple.second


-- TODO: rename "map"?
withModel : Env -> Model -> Env
withModel env model =
  { env | model = model }


-- TODO: rename "map"?
withModel2 : Env2 -> Model -> Env2
withModel2 env model =
  { env | model = model }
