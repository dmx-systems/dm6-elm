module Env exposing (Env, Env2, ExtManager, map, auto, from, autoSize, autoSize2)

import Model exposing (Model, Msg)
import ModelBase exposing (..)
import Outcome exposing (Outcome)
import Undo exposing (UndoModel)

import Html exposing (Html)



-- TYPES


{-| The environment the application passes to the modules.
TODO: drop
-}
type alias Env =
  { model : Model
  , undoModel : UndoModel
  , ext : ExtManager
  }


{-| The environment the dispatcher passes to the extensions/feature modules
TODO: rename "Env"
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
  , dragStop : DragStop
  --
  , all : Extensions
  }



-- Extension Points
--
-- Called by the **extension user** (by using the "ext" value).
-- Compare to ExtManager.elm


type alias Init =
  BoxId -> Model -> (BoxId -> Model -> Model)


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
  BoxId -> Point -> Model -> (Model, Maybe Target)


type alias DragStop =
  BoxId -> Model -> Outcome



-- HELPER


-- TODO: drop
autoSize : Env -> Model -> Model
autoSize {ext} model =
  model
    |> ext.autoSize [ model.boxId ]
    |> Tuple.second


-- TODO: drop
autoSize2 : Env2 -> Model -> Model
autoSize2 {ext} model =
  model
    |> ext.autoSize [ model.boxId ]
    |> Tuple.second


-- TODO: rename "autoSize"
-- TODO: return Env2?
auto : Env2 -> Model
auto ({model, ext} as env) =
  model
    |> ext.autoSize [ model.boxId ]
    |> Tuple.second


map : (Model -> Model) -> Env2 -> Env2
map transform ({model} as env) =
  { env | model = transform model }


from: Env -> Env2
from {model, ext} =
  Env2 model ext
