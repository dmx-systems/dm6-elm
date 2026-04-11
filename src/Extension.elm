module Extension exposing (view, hitTest)

import Box
import BoxRendererDef exposing (toName)
import ExtensionDef exposing (NestingBoxRenderer, NestingHitTest)
import Model exposing (Model, Msg)
import ModelBase exposing (Id, BoxId, BoxPath, Target, Point)
-- box renderers
import TopicList.TopicList
import TopicList.Geometry
import TopicMap.View
import TopicMap.Geometry

import Dict exposing (Dict)
import Html exposing (Html, text)



-- TYPES


type alias Renderer =
  { view : NestingBoxRenderer
  , hitTest : NestingHitTest
  }


-- VALUES


-- key = renderer name
registry : Dict String Renderer
registry =
  Dict.fromList -- Note: custom types can't be used as Dict keys, so we use String
    [ ("TopicMap",
        { view = TopicMap.View.view
        , hitTest = TopicMap.Geometry.hitTest
        }
      )
    , ("List",
        { view = TopicList.TopicList.view
        , hitTest = TopicList.Geometry.hitTest
        }
      )
    ]



--


{-| The dispatching box renderer.
Basically it takes a box ID and dispatches to the renderer (e.g. TopicMap, List) that is in
charge. By passing itself it enables a box renderer to call it for rendering nested boxes.
Note: structurally the dispatching box renderer *is* a box renderer: it takes a box ID and
returns HTML.
-}
view : BoxId -> BoxPath -> Model -> Html Msg
view boxId boxPath model =
  dispatch boxId model (text "Renderer ?")
    (\renderer -> renderer.view boxId boxPath view model)


hitTest : BoxId -> BoxPath -> Point -> Maybe Id -> Model -> Maybe Target
hitTest boxId boxPath pos excludeTopicId model =
  dispatch boxId model Nothing
    (\renderer -> renderer.hitTest boxId boxPath pos excludeTopicId hitTest model)


dispatch : BoxId -> Model -> r -> (Renderer -> r) -> r
dispatch boxId model errVal func =
  Box.rendererOf boxId model
    |> Maybe.andThen (\r -> registry |> Dict.get (toName r))
    |> Maybe.map func
    |> Maybe.withDefault errVal
