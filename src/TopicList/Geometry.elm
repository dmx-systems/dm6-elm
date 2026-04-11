module TopicList.Geometry exposing (hitTest, autoSize)

import Box
import ExtensionDef exposing (HitTest, AutoSize)
import Model exposing (Model)
import ModelBase exposing (Id, BoxId, BoxPath, Target, Point, Rectangle)
import Utils as U



-- Hit Test

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


-- Auto-Size

autoSize : BoxPath -> AutoSize -> Model -> (Rectangle, Model)
autoSize boxPath autoSize_ model =
  (Rectangle 0 0 240 100, model)
