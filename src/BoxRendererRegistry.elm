module BoxRendererRegistry exposing (view)

import Box
import BoxRenderer exposing (BoxRenderer, BoxView)
import BoxRendererDef exposing (toName)
-- installed box renderers
import TopicMap.View
import TopicList.TopicList

import Dict exposing (Dict)
import Html exposing (text)



-- VALUES


-- key = renderer name
registry : Dict String {view : BoxView}
registry =
  Dict.fromList
    [ ("TopicMap", {view = TopicMap.View.view})
    , ("List", {view = TopicList.TopicList.view})
    ]



-- VIEW


{-| The dispatching box renderer.
Basically it takes a box ID and dispatches to the renderer (e.g. TopicMap, List) that is in
charge. By passing itself it enables a box renderer to call it for rendering nested boxes.
Note: structurally the dispatching box renderer *is* a box renderer: it takes a box ID and
returns HTML.
-}
view : BoxRenderer
view boxId boxPath model =
  case Box.rendererOf boxId model of
    Just renderer ->
      case Dict.get (toName renderer) registry of
        Just renderFunc -> renderFunc.view view boxId boxPath model
        Nothing -> text "Renderer ??"
    Nothing -> text "Renderer ?"
