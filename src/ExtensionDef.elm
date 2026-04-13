module ExtensionDef exposing (ExtManager, Env, AutoSize)
-- TODO: don't expose AutoSize?

import Model exposing (Model, Msg)
import ModelBase exposing (Id, BoxId, BoxPath, Target, Point, Rectangle, Extensions)
import Undo exposing (UndoModel)

import Html exposing (Html)



-- TYPES


type alias ExtManager =
  { view : BoxRenderer
  , hitTest : HitTest
  , autoSize : AutoSize
  , all : Extensions
  }


type alias Env =
  { model : Model
  , undoModel : UndoModel
  , ext : ExtManager
  }


--

type alias BoxRenderer =
  BoxId -> BoxPath -> Model -> Html Msg


type alias HitTest =
  BoxId -> BoxPath -> Point -> Maybe Id -> Model -> Maybe Target


type alias AutoSize =
  BoxPath -> Model -> (Rectangle, Model)
