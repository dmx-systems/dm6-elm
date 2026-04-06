module BoxRendererRegistry exposing (view)

import Box
import BoxRenderer exposing (BoxRenderer, BoxView)
import BoxRendererDef exposing (toName)
-- box renderer modules
import TopicMap.View

import Dict exposing (Dict)
import Html exposing (text)



-- TYPES


type alias InstalledBoxRenderer =
  { view : BoxView }



-- VALUES


-- key = renderer name
registry : Dict String InstalledBoxRenderer
registry =
  Dict.fromList
    [ ("TopicMap", InstalledBoxRenderer TopicMap.View.view)
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
