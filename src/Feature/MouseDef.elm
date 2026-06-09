module Feature.MouseDef exposing (..)

import ModelBase exposing (..)



type alias Model =
  { dragState : Maybe DragState -- Initialzed on DownOnTopic, reset on Up (see Msg below).
  , hover : Maybe BoxTarget     -- The geometrically hovered topic. (Drop semantics is added by
                                -- particular renderer e.g. TopicMap.Mouse.updateTarget.)
                                -- Synthesized also on pointerdown for touch devices.
  }


init : Model
init =
  { dragState = Nothing
  , hover = Nothing
  }


resetTransient : Model -> Model
resetTransient _ =
  init


type alias DragState =
  { topicId : TopicId      -- the topic being dragged                     \
  , boxPath : BoxPath      -- direct topic context                        |
  , ixBoxPath : BoxPath    -- interaction box path - wider topic context  | constant while drag
  , startPos : Point       -- pointerdown position (client coordinates)   /
  , lastPointerPos : Point -- pointermove position (client coordinates)
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
