module TopicMap.Controller exposing (update)

import ModelBase exposing (..)
import Env exposing (Env)
import Feature.Sel as Sel
import Model exposing (Model, Msg)
import Storage as S
import TopicMap.TopicMap as TM
import TopicMap.Mouse as Mouse
import TopicMap.TopicMapDef as TopicMapDef
import Undo exposing (UndoModel)



-- UPDATE


update : TopicMapDef.Msg -> Env -> (UndoModel, Cmd Msg)
update msg ({model, undoModel} as env) =
  case msg of
    TopicMapDef.AssocClicked assocId boxPath -> selectAssoc assocId boxPath model
      |> Undo.swap undoModel
    TopicMapDef.GotTime time -> Mouse.timeArrived time undoModel
    TopicMapDef.GotRandomPos topicId boxId pos -> TM.addTopicAt topicId boxId pos env
      |> S.store |> Undo.push undoModel


selectAssoc : AssocId -> BoxPath -> Model -> (Model, Cmd Msg)
selectAssoc assocId boxPath model =
  ( model
      |> Sel.select (A assocId) boxPath
  , Cmd.none
  )
