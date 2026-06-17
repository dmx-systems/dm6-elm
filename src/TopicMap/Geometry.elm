module TopicMap.Geometry exposing (hitTest, autoSize)

import Box
import Config as C
import Env exposing (Env2, ExtManager)
import Feature.TextDef exposing (EditState(..))
import Model exposing (Model)
import ModelBase exposing (..)
import Topic
import TopicMap.BoxProps as TM
import TopicMap.TopicMapDef exposing (BoxProps, TopicProps)
import TopicMap.ViewModel as VM
import Utils as U



-- HIT TEST


-- ExtManager.ExtHitTest
{-| Finds the topic/box at a given screen position, if any.
Returns the found topic/box (Id) and its context (BoxPath), or Nothing.
If `maybeFilter` is given that topic/box will be excluded from search.

boxId - the Box to search in, result is one of its (deep) children or the box itself
boxPath - path of the Box to search in
localPos - the screen position, local to the Box to search in
-}
hitTest : BoxId -> BoxPath -> Point -> Maybe TopicId -> Env2 -> Maybe BoxTarget
hitTest (BoxId topicId as boxId) boxPath localPos maybeFilter ({model} as env) =
  case TM.byId boxId model of
    Just boxProps ->
      let
        topics = TM.allTopicProps boxProps model
        modelPos = toModelPos localPos boxProps
      in
      case testChildren modelPos topics (boxId :: boxPath) maybeFilter env of
        Just boxTarget -> Just boxTarget
        Nothing ->
          if Box.isFullscreen boxId model then
            Nothing
          else
            let
              parentBoxId = Box.firstId boxPath
            in
            if isBoxRectHit localPos boxProps parentBoxId model then
              Just (BoxTarget boxId (T topicId, boxPath))
            else
              Nothing
    Nothing -> Nothing


{-| Finds the topic/box at a given screen position within the given topics which all originate
from the same box, that is the 1st box in BoxPath.

modelPos - the screen position, in box-model coordinates
topics - the topics to search in (deep), result is one of these or their children
-}
testChildren : Point -> List TopicProps -> BoxPath -> Maybe TopicId -> Env2 -> Maybe BoxTarget
testChildren modelPos topics boxPath maybeFilter ({model, ext} as env) =
  case topics of
    [] -> Nothing
    topic :: tailTopics ->
      let
        boxId = Box.firstId boxPath
        isHeaderHit = isTopicHeaderHit modelPos topic.id boxId model
        isBox = Topic.isBox topic.id model
        expansion = Box.expansionOf topic.id boxId model
        ixBoxId = if isBox then BoxId topic.id else boxId
        -- partially applied
        isDetailHit = isTopicDetailHit modelPos topic.id boxId
        continueTest = testChildren modelPos tailTopics boxPath maybeFilter -- recursion
        relPos = relPos_ modelPos topic.id boxPath
        --
        maybeBoxTarget =
          case (isBox, expansion, isHeaderHit) of
            (_, _, True) -> result True
            (_, Collapsed, False) -> result False
            (False, Expanded, False) -> result (isDetailHit model)
            (True, Expanded, False) -> -- recursion
              ext.hitTest (BoxId topic.id) boxPath (relPos model) maybeFilter model
        -- if positive returns current topic as the result
        result : Bool -> Maybe BoxTarget
        result found =
          if found then
            Just (BoxTarget ixBoxId (T topic.id, boxPath))
          else
            Nothing
      in
      -- return topic if successfully tested AND not excluded by filter,
      -- otherwise continue testing
      case maybeBoxTarget of
        Just {target} ->
          case (target, maybeFilter) of
            ((itemId, _), Just filterTopicId) ->
              case itemId /= T filterTopicId of
                True -> maybeBoxTarget
                False -> continueTest env
            (_, Nothing) -> maybeBoxTarget
        Nothing -> continueTest env


{-| Transforms a box-local (screen) position to box-model coordinates, involving both the box's
coordinate system translation (rect) and the box's scroll values.
-}
toModelPos : Point -> BoxProps -> Point
toModelPos pos boxProps =
  Point
    (pos.x + boxProps.rect.x1 + boxProps.scroll.x)
    (pos.y + boxProps.rect.y1 + boxProps.scroll.y)
    -- FIXME: don't apply scroll value of nested boxes?


-- TODO: function name, description
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


-- ExtManager.ExtAutoSize
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
          case model.mouse.dragSource of
            Just dragSource ->
              let
                dragPath = dragSource.boxPath
              in
              (True
              , (dragPath |> List.drop (List.length dragPath - List.length boxPath)) == boxPath
              , List.member boxId dragPath
              )
            Nothing -> (False, False, False)
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
