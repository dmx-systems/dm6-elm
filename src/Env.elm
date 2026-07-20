module Env exposing (Env, Dispatch, map, autoSize, outcome, outcomeDir, outcomeCmd)

import Model exposing (Model, Msg)
import ModelBase exposing (..)
import Outcome exposing (..)

import Html exposing (Html)



-- TYPES


{-| The environment the dispatcher passes to the extensions/feature modules
TODO: rename "Env" ### FIXDOC
-}
type alias Env =
  { model : Model
  , dispatch : Dispatch
  }


{-| A value of this type is exported as "dispatch" by Dispatch module.
-}
type alias Dispatch =
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
-- The functions of the "dispatch" value (as imported from Dispatch) have these types.
-- Called by the **extension user**.
-- Compare to Dispatch.elm


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
        |> env.dispatch.autoSize [ model.boxId ]
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
  Outcome env.model Cmd.none (Directives NoStore Swap)


outcomeDir : Directives -> Env -> Outcome
outcomeDir directives env =
  Outcome env.model Cmd.none directives


outcomeCmd : Cmd Msg -> Env -> Outcome
outcomeCmd cmd env =
  Outcome env.model cmd (Directives NoStore Swap)
