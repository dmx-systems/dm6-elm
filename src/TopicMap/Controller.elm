module TopicMap.Controller exposing (update)

import Env exposing (Env)
import Model exposing (Msg)
import Storage as S
import TopicMap.BoxProps as TM
import TopicMap.Mouse as Mouse
import TopicMap.TopicMapDef as TopicMapDef
import Undo exposing (UndoModel)



-- UPDATE


update : TopicMapDef.Msg -> Env -> (UndoModel, Cmd Msg)
update msg ({model, undoModel} as env) =
  case msg of
    TopicMapDef.Time time -> Mouse.timeArrived time undoModel
    TopicMapDef.GotRandomPos topicId boxId pos -> TM.addTopicAt topicId boxId pos env
      |> S.store |> Undo.push undoModel
