module Feature.MouseDef exposing (..)

import ModelBase exposing (..)



type alias Model =
  -- Tracks what topic is dragged, the "source".
  -- Initialzed on DownOnTopic, reset on Up (see Msg below).
  { dragSource : Maybe DragSource
  -- The geometrically hovered topic. (Drop target semantics is added
  -- by respective renderers, e.g. TopicMap.Mouse.updateTarget.)
  -- Synthesized also on pointerdown for touch devices.
  , hover : Maybe BoxTarget
  }


init : Model
init =
  { dragSource = Nothing
  , hover = Nothing
  }


resetTransient : Model -> Model
resetTransient _ =
  init


type alias DragSource =
  { topicId : TopicId      -- the topic being dragged                     \
  , boxPath : BoxPath      -- direct topic context                        |
  , ixBoxPath : BoxPath    -- interaction box path - wider topic context  | constant while drag
  , startPos : Point       -- pointerdown position (client coordinates)   /
  , lastPointerPos : Point -- pointermove position (client coordinates) -- rename lastClientPos?
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
