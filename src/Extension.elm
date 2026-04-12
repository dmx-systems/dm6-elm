module Extension exposing (ext)

import Box
import ExtensionDef exposing (..)
import Model exposing (Model, Msg)
import ModelBase exposing (..)
-- box renderers
import TopicList.Geometry
import TopicList.TopicList
import TopicMap.Geometry
import TopicMap.Size
import TopicMap.View

import Dict exposing (Dict)
import Html exposing (Html, text)



-- TYPES


type alias Extension =
  { name : ExtName
  , label : ExtLabel
  , view : NestingBoxRenderer
  , hitTest : NestingHitTest
  , autoSize : NestingAutoSize
  }



-- VALUES


ext : ExtManager
ext =
  { view = view
  , hitTest = hitTest
  , autoSize = autoSize
  , all = all
  }


-- key = renderer name
registry : Dict String Extension
registry =
  Dict.fromList -- Note: custom types can't be used as Dict keys, so we use String
    [ ("TopicMap",
        { name = "TopicMap"
        , label = "Topic Map"
        , view = TopicMap.View.view
        , hitTest = TopicMap.Geometry.hitTest
        , autoSize = TopicMap.Size.autoSize
        }
      )
    , ("List",
        { name = "List"
        , label = "List"
        , view = TopicList.TopicList.view
        , hitTest = TopicList.Geometry.hitTest
        , autoSize = TopicList.Geometry.autoSize
        }
      )
    ]


all : Extensions
all =
  registry
    |> Dict.values
    |> List.map
      (\{name, label} -> (name, label))



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
    (\renderer -> renderer.view boxId boxPath view all model)


hitTest : BoxId -> BoxPath -> Point -> Maybe Id -> Model -> Maybe Target
hitTest boxId boxPath pos excludeTopicId model =
  dispatch boxId model Nothing
    (\renderer -> renderer.hitTest boxId boxPath pos excludeTopicId hitTest model)


autoSize : BoxPath -> Model -> (Rectangle, Model)
autoSize boxPath model =
  dispatch (Box.firstId boxPath) model (Rectangle 0 0 0 0, model)
    (\renderer -> renderer.autoSize boxPath autoSize model)


dispatch : BoxId -> Model -> result -> (Extension -> result) -> result
dispatch boxId model errVal func =
  Box.rendererOf boxId model
    |> Maybe.andThen (\renderer -> registry |> Dict.get renderer)
    |> Maybe.map func
    |> Maybe.withDefault errVal
