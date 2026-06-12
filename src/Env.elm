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


{-| A value of this type is exported as "ext" by ExtManager module.
-}
type alias ExtManager =
  { init : Init
  , view : BoxRenderer
  , hitTest : HitTest
  , autoSize : AutoSize
  -- Drag and Drop
  , dragStart : DragStart
  , drag : Drag
  , updateDropTarget : DropTargeting
  , resetDropTarget : DropTargetReset
  , dragStop : DragStop
  --
  , addTopic : AddTopic
  , all : Extensions
  }


-- Extension Points
--
-- Called on the "ext" value by the application developer.

type alias Init =
  BoxId -> Model -> Model


type alias BoxRenderer =
  BoxId -> BoxPath -> Model -> Html Msg


-- Point is in box-local coordinates
type alias HitTest =
  BoxId -> BoxPath -> Point -> Maybe TopicId -> Model -> Maybe BoxTarget


type alias AutoSize =
  BoxPath -> Model -> (Rectangle, Model)


-- Drag and Drop

-- Note: no drag specific parameters here. An extension's "dragStart" handler operates on
-- (feature module) Mouse's "dragSource" directly.
type alias DragStart =
  Model -> (Model, Cmd Msg)


type alias Drag =
  BoxId -> Point -> Model -> (Model, Cmd Msg)


type alias DropTargeting =
  BoxId -> Point -> Model -> Model


type alias DropTargetReset =
  BoxId -> Model -> Model


type alias DragStop =
  BoxId -> Model -> Outcome


--

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
