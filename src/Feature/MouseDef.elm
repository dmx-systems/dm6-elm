module Feature.MouseDef exposing (..)

import ModelBase exposing (..)



type alias Model =
  -- The geometrically hovered topic, if any.
  -- Synthesized also on pointerdown for touch devices.
  -- TODO: meanwhile a box's renderer is set deep. Is ixBoxPath still needed in hover target?
  { hover : Maybe BoxTarget -- transient
  -- The topic being dragged.
  -- Available while drag is active (initialzed on DownOnTopic, reset on Up).
  , dragSource : Maybe DragSource -- transient
  -- While drag is active: the drop target as accepted by currently hovered renderer,
  -- e.g. TopicMap, TopicList. Note: additional renderer specific drop state might be
  -- hold by the respective renderer.
  , dropTarget : Maybe Target -- transient
  }


init : Model
init =
  { hover = Nothing
  , dragSource = Nothing
  , dropTarget = Nothing
  }


resetTransient : Model -> Model
resetTransient _ =
  init


type alias DragSource =
  { topicId : TopicId      -- the topic being dragged                     \
  , boxPath : BoxPath      -- direct topic context                        |
  , ixBoxPath : BoxPath    -- interaction box path - wider topic context  | constant while drag
  , startPos : Point       -- pointerdown position (client coordinates)   /
  , lastPointerPos : Point -- pointermove position (client coordinates) -- rename lastPos?
  }


type Msg
  -- Topic dragging
  = DownOnTopic TopicId BoxPath BoxPath (Point, PointerType)
                              -- pointerdown on topic (Events.draggable)
                              -- data: 2nd path is "interaction box path"
  | Move (Point, PointerType) -- (fired by handlers created by Events.globalMouseHandler)
  | Up                        -- (fired by handlers created by Events.globalMouseHandler)
  -- UI cancellation
  | Cancel -- pointerdown somewhere (fired by handlers created by Events.globalMouseHandler)
  | Leave
