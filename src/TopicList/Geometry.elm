module TopicList.Geometry exposing (hitTest)

import Box
import ExtensionDef exposing (HitTest)
import Model exposing (Model)
import ModelBase exposing (Id, BoxId, BoxPath, Target, Point)
import Utils as U



hitTest : BoxId -> BoxPath -> Point -> Maybe Id -> HitTest -> Model -> Maybe Target
hitTest boxId boxPath pos excludeTopicId geometry model =
  let
    maybeThisItem : Bool -> Maybe Target
    maybeThisItem found = if found then Just (boxId, boxPath) else Nothing
  in
  -- test this map's items either if the map is displayed fullscreen, or it is whiteboxed
  if Box.isFullscreen boxId model then
    Nothing
  else
    isListHovered pos |> maybeThisItem


isListHovered : Point -> Bool
isListHovered pos =
  -- TODO: item pos, item size, display mode, topic header
  pos.x > 0 &&
  pos.x < 240 &&
  pos.y > 0 &&
  pos.y < 100
