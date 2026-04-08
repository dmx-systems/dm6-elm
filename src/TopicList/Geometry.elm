module TopicList.Geometry exposing (findTopicAt)

import BoxRenderer exposing (TopicGeometry)
import Model exposing (Model)
import ModelBase exposing (Id, BoxPath, Point)



findTopicAt : TopicGeometry -> Point -> Maybe Id -> Model -> Maybe (Id, BoxPath)
findTopicAt geometry pos excludeTopicId model =
  Nothing -- TODO
