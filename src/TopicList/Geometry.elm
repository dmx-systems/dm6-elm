module TopicList.Geometry exposing (findTopicAt)

import BoxRenderer exposing (BoxGeometry)
import Model exposing (Model)
import ModelBase exposing (Id, BoxPath, Point)



findTopicAt : Point -> Maybe Id -> BoxGeometry -> Model -> Maybe (Id, BoxPath)
findTopicAt pos excludeTopicId geometry model =
  Nothing -- TODO
