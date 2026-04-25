module Env exposing (ExtManager, Env, Env2, autoSize, withModel)
-- TODO: don't expose AutoSize?

import Model exposing (Model, Msg)
import ModelBase exposing (Id, BoxId, BoxPath, Target, Point, Rectangle, Extensions, ToolbarPos)
import Undo exposing (UndoModel)

import Html exposing (Html)



-- TYPES


type alias ExtManager =
  { view : BoxRenderer
  , hitTest : HitTest
  , autoSize : AutoSize
  , toolbar : Toolbar
  , all : Extensions
  }


{-| The environment the application passes to the modules -}
type alias Env =
  { model : Model
  , undoModel : UndoModel
  , ext : ExtManager
  }


{-| The environment the dispatcher passes to the extensions -}
type alias Env2 =
  { model : Model
  , ext : ExtManager
  }


-- Extension capabilities

type alias BoxRenderer =
  BoxId -> BoxPath -> Model -> Html Msg


type alias HitTest =
  BoxId -> BoxPath -> Point -> Maybe Id -> Model -> Maybe Target


type alias AutoSize =
  BoxPath -> Model -> (Rectangle, Model)


type alias Toolbar =
  BoxId -> Model -> ToolbarPos



-- HELPER


autoSize : Env -> Model -> Model
autoSize {ext} model =
  model
    |> ext.autoSize [ model.boxId ]
    |> Tuple.second


withModel : Env -> Model -> Env
withModel env model =
  { env | model = model }
