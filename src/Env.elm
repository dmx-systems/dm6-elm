module Env exposing (Env, Dispatch, Renderers, RendererLabel, map, autoSize, outcome,
  outcomeDir, outcomeCmd)

import Model exposing (Model, Msg)
import ModelBase exposing (..)
import Outcome exposing (..)

import Html exposing (Html)



-- TYPES


{-| The environment as passed by Main.update to respective module update functions (both box
renderers and feature modules).
Env does not know the installed renderers, only the Dispatch module knows.
-}
type alias Env =
  { model : Model
  , dispatch : Dispatch
  }


{-| The dispatcher "interface" as implemented by Dispatch.elm module.
A value of this type is created by the Dispatch module and exported as "dispatch".
-}
type alias Dispatch =
  -- Dispatch functions
  { init : Init
  , view : View
  , hitTest : HitTest
  , autoSize : AutoSize
  , dragStart : DragStart
  , drag : Drag
  , dragAccept : DragAccept
  , dragStop : DragStop
  -- List of all renderers
  , all : Renderers
  }


type alias Renderers = List (RendererName, RendererLabel)
type alias RendererName = String
type alias RendererLabel = String


-- Dispatch function types as implemented by Dispatch.elm

type alias Init =
  BoxId -> Model -> (BoxId -> Model -> Model)


type alias View =
  BoxId -> BoxPath -> Model -> Html Msg


-- Point is in box-local coordinates
type alias HitTest =
  BoxId -> BoxPath -> Point -> Maybe TopicId -> Model -> Maybe BoxTarget


-- TODO: return just Model
type alias AutoSize =
  BoxPath -> Model -> (Rectangle, Model)


-- Drag and Drop

-- Note: no drag specific parameters here. A renderer's "dragStart" handler operates on
-- (feature module) Mouse's "dragSource" state.
type alias DragStart =
  Model -> (Model, Cmd Msg)


type alias Drag =
  BoxId -> Point -> Model -> (Model, Cmd Msg)


type alias DragAccept =
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
