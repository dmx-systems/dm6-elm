module TopicMap.Geometry exposing (hitTest, autoSize, toolbarPos)

import Box
import Config as C
import Env exposing (Env2, ExtManager)
import Feature.MouseDef exposing (DragState(..))
import Feature.TextDef exposing (EditState(..))
import Model exposing (Model)
import ModelBase exposing (..)
import Topic
import TopicMap.BoxProps as TM
import TopicMap.TopicMapDef exposing (BoxProps, TopicProps, MouseState(..))
import TopicMap.ViewModel as VM
import Utils as U



-- HIT TEST


-- ExtManager.NestingHitTest
{-| Finds the topic/box at a given screen position.
Returns the found topic/box (Id) and its context (BoxPath), or Nothing.
If `excludeTopicId` is given that topic/box will be excluded from search.
-}
hitTest : BoxId -> BoxPath -> Point -> Maybe TopicId -> Env2 -> Maybe Target
hitTest (BoxId topicId as boxId) boxPath pos excludeTopicId ({model} as env) =
  case TM.byId boxId model of
    Just boxProps ->
      let
        topics = TM.allTopicProps boxProps model
        relPos = mapOffset pos boxProps
      in
      case testChildren relPos topics (boxId :: boxPath) excludeTopicId env of
        Just target -> Just target
        Nothing ->
          if Box.isFullscreen boxId model then
            Nothing
          else
            let
              parentBoxId = Box.firstId boxPath
            in
            if isBoxRectHit pos boxProps parentBoxId model then
              Just (T topicId, boxPath)
            else
              Nothing
    Nothing -> Nothing


testChildren : Point -> List TopicProps -> BoxPath -> Maybe TopicId -> Env2 -> Maybe Target
testChildren pos topics boxPath excludeTopicId ({model, ext} as env) =
  case topics of
    [] -> Nothing
    topic :: tailTopics ->
      let
        maybeItem : Bool -> Maybe Target
        maybeItem found = if found then Just (T topic.id, boxPath) else Nothing
        --
        boxId = Box.firstId boxPath
        isHeaderHit = isTopicHeaderHit pos topic.id boxId >> maybeItem
        relPos = relPos_ pos topic.id boxPath
        maybeTarget =
          case (Topic.isBox topic.id model, Box.expansionOf topic.id boxId model) of
            (True, Collapsed) -> isHeaderHit model
            (True, Expanded) ->
              case ext.hitTest (BoxId topic.id) boxPath (relPos model) excludeTopicId model of
                Just target -> Just target
                Nothing -> isHeaderHit model
            (False, _) -> isTopicHit topic.id boxPath pos model |> maybeItem
        testTailItems = testChildren pos tailTopics boxPath excludeTopicId -- recursion
      in
      -- return topic if successfully tested AND not excluded by filter
      case (maybeTarget, excludeTopicId) of
        (Just ((targetId, _) as target), Just topicId) ->
          case targetId /= T topicId of
            True -> Just target
            False -> testTailItems env
        (Just target, Nothing) -> Just target
        (Nothing, _) -> testTailItems env


isTopicHit : TopicId -> BoxPath -> Point -> Model -> Bool
isTopicHit itemId boxPath pos model =
  let
    boxId = Box.firstId boxPath -- Note: a topic is never displayed fullscreen
    isHeaderHit = isTopicHeaderHit pos itemId boxId
    isDetailHit = isTopicDetailHit pos itemId boxId
  in
  -- test depends on topic's expansion
  case Box.expansionOf itemId boxId model of
    Collapsed -> isHeaderHit model
    Expanded -> isHeaderHit model || isDetailHit model


{-| Transforms the given screen position to a map-relative position according to the given map.
-}
mapOffset : Point -> BoxProps -> Point
mapOffset pos boxProps =
  Point
    (pos.x + boxProps.rect.x1 + boxProps.scroll.x)
    (pos.y + boxProps.rect.y1 + boxProps.scroll.y)


relPos_ : Point -> TopicId -> BoxPath -> Model -> Point
relPos_ pos topicId boxPath model =
  case TM.topicPos topicId (Box.firstId boxPath) model of
    Just boxPos ->
      Point
        (pos.x - boxPos.x + C.topicW2)
        (pos.y - boxPos.y - C.topicH2)
    Nothing -> pos


-- TODO: factor out common BoxProps.Size code

isTopicHeaderHit : Point -> TopicId -> BoxId -> Model -> Bool
isTopicHeaderHit pos topicId boxId model =
  case TM.topicPos topicId boxId model of
    Just topicPos ->
      pos.x > topicPos.x - C.topicW2 - C.topicHeight && -- left edge includes caret area
      pos.x < topicPos.x + C.topicW2 &&
      pos.y > topicPos.y - C.topicH2 &&
      pos.y < topicPos.y + C.topicH2
    Nothing -> False


isTopicDetailHit : Point -> TopicId -> BoxId -> Model -> Bool
isTopicDetailHit pos topicId boxId model =
  case (TM.topicPos topicId boxId model, Topic.size topicId .view model) of
    (Just topicPos, Just size) ->
      pos.x > topicPos.x - C.topicW2 + C.topicHeight && -- topicHeight = icon box width
      pos.x < topicPos.x - C.topicW2 + C.topicHeight + size.w &&
      pos.y > topicPos.y - C.topicH2 &&
      pos.y < topicPos.y - C.topicH2 + size.h
    _ -> False


isBoxRectHit : Point -> BoxProps -> BoxId -> Model -> Bool
isBoxRectHit pos boxProps parentBoxId model =
  case TM.topicPos (fromBoxId boxProps.id) parentBoxId model of
    Just boxPos ->
      pos.x > 0 &&
      pos.x < boxProps.rect.x2 - boxProps.rect.x1 &&
      pos.y > 0 &&
      pos.y < boxProps.rect.y2 - boxProps.rect.y1
    Nothing -> False



-- AUTO-SIZE


-- ExtManager.NestingAutoSize
{-| Calculates the TopicMap's "rect" (recursively) and modifies the model accordingly.
Returns the modified model along with, for convenience, the calculated rect.
Based on the rect's change the TopicMap's topic position adjustment within the parent
TopicMap (if any) is calculated as well.
-}
autoSize : BoxPath -> Env2 -> (Rectangle, Model)
autoSize boxPath ({model, ext}) =
  let
    boxId = Box.firstId boxPath
  in
  case TM.byId boxId model of
    Just boxProps ->
      let
        topics = VM.topicsToRender boxProps model
        (rect, model_) =
          if topics |> List.isEmpty then
            (C.whiteBoxEmpty, model)
          else
            topics |> List.foldr
              (\mapItem (rectAcc, modelAcc) ->
                accumulateItem mapItem boxPath rectAcc ext modelAcc
              )
              (Rectangle 0 0 0 0, model)
        newRect = addBoxPadding rect
      in
      ( newRect
      , updateBoxGeometry boxPath newRect boxProps.rect model_
      )
    Nothing -> (Rectangle 0 0 0 0, model)


accumulateItem : TopicProps -> BoxPath -> Rectangle -> ExtManager -> Model -> (Rectangle, Model)
accumulateItem mapItem boxPath rectAcc ext model =
  let
    (rect, model_) = calcItemRect mapItem boxPath ext model
  in
  (accumulateRect rectAcc rect, model_)


calcItemRect : TopicProps -> BoxPath -> ExtManager -> Model -> (Rectangle, Model)
calcItemRect ({pos, expansion} as topic) boxPath ext model =
  case (Topic.isBox topic.id model, expansion) of
    (False, Collapsed) -> (topicExtent pos, model)
    (False, Expanded) -> (detailTopicExtent topic.id boxPath pos model, model)
    (True, Collapsed) -> (topicExtent pos, model)
    (True, Expanded) ->
      let
        (rect_, model_) = ext.autoSize (BoxId topic.id :: boxPath) model -- recursion
      in
      (boxExtent pos rect_, model_)


topicExtent : Point -> Rectangle
topicExtent pos =
  Rectangle
    (pos.x - C.topicW2)
    (pos.y - C.topicH2)
    (pos.x + C.topicW2 + 2 * C.topicBorderWidth)
    (pos.y + C.topicH2 + 2 * C.topicBorderWidth)


detailTopicExtent : TopicId -> BoxPath -> Point -> Model -> Rectangle
detailTopicExtent topicId boxPath pos model =
  let
    isEdit = model.text.edit == Edit topicId boxPath -- TODO: use Text (cyclic atm)
    get = if isEdit then .editor else .view
    maybeSize =
      case Topic.size topicId get model of
        Just size ->
          if isEdit then
            Just { size | w = C.topicDetailMaxWidth }
          else
            Just size
        Nothing -> Nothing
  in
  case maybeSize of
    Just size ->
      Rectangle
        (pos.x - C.topicW2)
        (pos.y - C.topicH2)
        (pos.x - C.topicW2 + size.w + C.topicHeight + 2 * C.topicBorderWidth)
        (pos.y - C.topicH2 + size.h + 2 * C.topicBorderWidth)
    Nothing -> Rectangle 0 0 0 0 -- error is logged already


boxExtent : Point -> Rectangle -> Rectangle
boxExtent pos rect =
  let
    boxWidth = rect.x2 - rect.x1
    boxHeight = rect.y2 - rect.y1
  in
  Rectangle
    (pos.x - C.topicW2)
    (pos.y - C.topicH2)
    (pos.x - C.topicW2 + boxWidth)
    (pos.y + C.topicH2 + boxHeight)


accumulateRect : Rectangle -> Rectangle -> Rectangle
accumulateRect rectAcc rect =
  Rectangle
    (min rectAcc.x1 rect.x1)
    (min rectAcc.y1 rect.y1)
    (max rectAcc.x2 rect.x2)
    (max rectAcc.y2 rect.y2)


addBoxPadding : Rectangle -> Rectangle
addBoxPadding rect =
  Rectangle
    (rect.x1 - C.whiteBoxPadding)
    (rect.y1 - C.whiteBoxPadding)
    (rect.x2 + C.whiteBoxPadding)
    (rect.y2 + C.whiteBoxPadding)


{-| Sets the box's "newRect" and, based on its change, calculates and sets the box's "pos"
adjustment ("delta")
-}
updateBoxGeometry : BoxPath -> Rectangle -> Rectangle -> Model -> Model
updateBoxGeometry boxPath newRect oldRect model =
  case boxPath of
    boxId :: parentBoxId :: _ ->
      let
        (isDragInProgress, isOnDragPath, isBoxInDragPath) =
          case model.mouse.dragState of
            DragStarted _ dragPath _ _ ->
              (True
              , (dragPath |> List.drop (List.length dragPath - List.length boxPath)) == boxPath
              , List.member boxId dragPath
              )
            _ -> (False, False, False)
      in
      if isDragInProgress then
        if isOnDragPath then
          model
            |> setBoxRect boxId newRect
            |> adjustBoxPos boxId parentBoxId newRect oldRect
            -- if boxes are revealed more than once only those within the drag-path
            -- get the position adjustment, the other box's positions remain stable
        else
          if isBoxInDragPath then
            model
            -- do nothing, postpone box's geometry update until reaching drag-path,
            -- otherwise, when reaching drag-path, the box's rect would be updated
            -- already and position adjustment would calculate 0
          else
            model |> setBoxRect boxId newRect
      else
        model |> setBoxRect boxId newRect
    [ boxId ] ->
      model |> setBoxRect boxId newRect
    [] -> U.logError "updateBoxGeometry" "boxPath is empty!" model


setBoxRect : BoxId -> Rectangle -> Model -> Model
setBoxRect boxId rect model =
  model
    |> TM.updateRect boxId (\_ -> rect)


adjustBoxPos : BoxId -> BoxId -> Rectangle -> Rectangle -> Model -> Model
adjustBoxPos (BoxId topicId) parentBoxId newRect oldRect model =
  model
    |> TM.updateTopicPos topicId parentBoxId
      (\oldPos ->
        (Point
          (oldPos.x + newRect.x1 - oldRect.x1)
          (oldPos.y + newRect.y1 - oldRect.y1)
        )
      )



-- TOOLBAR


-- ExtManager.NestingToolbar
toolbarPos : BoxPath -> Model -> ToolbarPos
toolbarPos boxPath model =
  let
    boxId = Box.firstId boxPath
  in
  case TM.byId boxId model of
    Just {rect} ->
      ToolbarPos
        (\topic ->
          case TM.topicPos topic.id boxId model of
            Just topicPos ->
              Point
                (topicPos.x - rect.x1 - C.topicW2)
                (topicPos.y - rect.y1 - C.topicH2 - 29) -- TODO: 29 ≈ toolbar height
            Nothing -> Point 0 0
        )
        (\assoc ->
          case TM.assocGeometry assoc boxId model of
            Just (p1, p2) ->
              Point
                ((p1.x + p2.x) // 2 - rect.x1 - 32) -- TODO: 32 ≈ toolbar width / 2
                ((p1.y + p2.y) // 2 - rect.y1 - 13) -- TODO: 13 ≈ toolbar height / 2
            Nothing -> Point 0 0
        )
    Nothing ->
      (ToolbarPos (\_ -> Point 0 0) (\_ -> Point 0 0))
