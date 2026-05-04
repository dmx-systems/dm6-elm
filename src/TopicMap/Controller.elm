module TopicMap.Controller exposing (update)

import Env exposing (Env)
import Model exposing (Msg)
import Storage as S
import TopicMap.Mouse as Mouse
import TopicMap.TopicMap as TM
import TopicMap.TopicMapDef as TopicMapDef
import Undo exposing (UndoModel)



-- UPDATE


update : TopicMapDef.Msg -> Env -> (UndoModel, Cmd Msg)
update msg ({model, undoModel} as env) =
  case msg of
    -- Topic Dragging
    TopicMapDef.DownOnTopic topicId boxPath (pos, pointerType) ->
      Mouse.mouseDownOnTopic topicId boxPath pos pointerType model |> Undo.swap undoModel
    TopicMapDef.Time time -> Mouse.timeArrived time undoModel
    -- TopicMap
    TopicMapDef.GotRandomPos topicId mapId pos -> TM.addTopic_ topicId mapId pos env
      |> S.store |> Undo.push undoModel
