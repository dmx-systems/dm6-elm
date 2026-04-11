module Extension exposing (view, hitTest, autoSize)

import Box
import BoxRendererDef exposing (toName)
import ExtensionDef exposing (NestingBoxRenderer, NestingHitTest, NestingAutoSize)
import Model exposing (Model, Msg)
import ModelBase exposing (Id, BoxId, BoxPath, Target, Point, Rectangle)
-- box renderers
import TopicList.Geometry
import TopicList.TopicList
import TopicMap.Geometry
import TopicMap.Size
import TopicMap.View

import Dict exposing (Dict)
import Html exposing (Html, text)



-- TYPES


type alias Renderer =
  { view : NestingBoxRenderer
  , hitTest : NestingHitTest
  , autoSize : NestingAutoSize
  }


-- VALUES


-- key = renderer name
registry : Dict String Renderer
registry =
  Dict.fromList -- Note: custom types can't be used as Dict keys, so we use String
    [ ("TopicMap",
        { view = TopicMap.View.view
        , hitTest = TopicMap.Geometry.hitTest
        , autoSize = TopicMap.Size.autoSize
        }
      )
    , ("List",
        { view = TopicList.TopicList.view
        , hitTest = TopicList.Geometry.hitTest
        , autoSize = TopicList.Geometry.autoSize
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


autoSize : BoxPath -> Model -> (Rectangle, Model)
autoSize boxPath model =
  dispatch (Box.firstId boxPath) model (Rectangle 0 0 0 0, model)
    (\renderer -> renderer.autoSize boxPath autoSize model)


dispatch : BoxId -> Model -> r -> (Renderer -> r) -> r
dispatch boxId model errVal func =
  Box.rendererOf boxId model
    |> Maybe.andThen (\r -> registry |> Dict.get (toName r))
    |> Maybe.map func
    |> Maybe.withDefault errVal
