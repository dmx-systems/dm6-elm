module TopicMap.Controller exposing (update)

import ModelBase exposing (..)
import Env exposing (Env)
import Feature.Sel as Sel
import Model exposing (Model)
import Outcome exposing (..)
import TopicMap.Mouse as Mouse
import TopicMap.TopicMap as TopicMap
import TopicMap.TopicMapDef as TopicMapDef



-- UPDATE


update : TopicMapDef.Msg -> Env -> Outcome
update msg ({model} as env) =
  case msg of
    TopicMapDef.AssocClicked assocId boxPath ->
      model
        |> selectAssoc assocId boxPath
        |> Outcome.default
    TopicMapDef.GotTime time ->
      model
        |> Mouse.timeArrived time
    TopicMapDef.GotRandomPos topicId boxId pos ->
      env
        |> TopicMap.addTopicAt topicId boxId pos
        |> Env.outcomeWith (Directives Store Push)


selectAssoc : AssocId -> BoxPath -> Model -> Model
selectAssoc assocId boxPath model =
  model
    |> Sel.select (A assocId) boxPath
