module Env exposing (Env, ExtManager, map, autoSize, outcome, outcomeWith, outcomeWithCmd)

import Model exposing (Model, Msg)
import ModelBase exposing (..)
import Outcome exposing (Outcome, Directives, Storage(..), History(..))
import Undo exposing (UndoModel)

import Html exposing (Html)



-- TYPES


{-| The environment the dispatcher passes to the extensions/feature modules
TODO: rename "Env" ### FIXDOC
-}
type alias Env =
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
-- The functions of the "ext" value (as imported from ExtManager) have these types.
-- Called by the **extension user**.
-- Compare to ExtManager.elm


type alias Init =
  BoxId -> Model -> (BoxId -> Model -> Model)


type alias BoxRenderer =
  BoxId -> BoxPath -> Model -> Html Msg


-- Point is in box-local coordinates
type alias HitTest =
  BoxId -> BoxPath -> Point -> Maybe TopicId -> Model -> Maybe BoxTarget


-- TODO: return just Model
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


autoSize : Env -> Env
autoSize env =
  let
    autoSize_ : Model -> Model
    autoSize_ model =
      model
        |> env.ext.autoSize [ model.boxId ]
        |> Tuple.second
  in
  env
    |> map autoSize_


map : (Model -> Model) -> Env -> Env
map transform ({model} as env) =
  { env | model = transform model }


-- Outcome

outcome : Env -> Outcome
outcome  env =
  Outcome (Directives NoStore Swap) Cmd.none env.model


outcomeWith : Directives -> Env -> Outcome
outcomeWith directives env =
  Outcome directives Cmd.none env.model


outcomeWithCmd : Cmd Msg -> Env -> Outcome
outcomeWithCmd cmd env =
  Outcome (Directives NoStore Swap) cmd env.model
