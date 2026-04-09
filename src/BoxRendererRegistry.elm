module BoxRendererRegistry exposing (view, findTopicAt)

import Box
import BoxRenderer exposing (..)
import BoxRendererDef exposing (toName)
import Model exposing (Model, Msg)
import ModelBase exposing (Id, BoxId, BoxPath, Point)
-- installed box renderers
import TopicMap.View
import TopicMap.Geometry
import TopicList.TopicList
import TopicList.Geometry

import Dict exposing (Dict)
import Html exposing (Html, text)



-- TYPES


type alias Renderer =
  { view : NestingBoxRenderer
  , findTopicAt : NestingBoxGeometry
  }


-- VALUES


-- key = renderer name
registry : Dict String Renderer
registry =
  Dict.fromList -- Note: custom types can't be used as Dict keys, so we use String
    [ ("TopicMap",
        { view = TopicMap.View.view
        , findTopicAt = TopicMap.Geometry.findTopicAt
        }
      )
    , ("List",
        { view = TopicList.TopicList.view
        , findTopicAt = TopicList.Geometry.findTopicAt
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


findTopicAt : Point -> Maybe Id -> Model -> Maybe (Id, BoxPath)
findTopicAt pos excludeTopicId model =
  dispatch model.boxId model Nothing
    (\renderer -> renderer.findTopicAt pos excludeTopicId findTopicAt model)


dispatch : BoxId -> Model -> r -> (Renderer -> r) -> r
dispatch boxId model errVal func =
  Box.rendererOf boxId model
    |> Maybe.andThen (\r -> registry |> Dict.get (toName r))
    |> Maybe.map func
    |> Maybe.withDefault errVal
