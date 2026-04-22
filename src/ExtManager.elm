module ExtManager exposing (ext)

import Box
import Env exposing (ExtManager)
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
  , toolbar : NestingToolbar
  }


{-| A box renderer transformation that enables the dispatching box renderer to inject itself
into the actual box renderer (e.g. TopicMap, List). ### FIXDOC
The actual renderers "view" functions are of this type.
Note: the actual box renderers get access to the dispatching box renderer as an argument of
their "view" function instead of importing a module. This avoids circular dependencies in
conjunction with recursively nested renderers.
-}
type alias NestingBoxRenderer =
  BoxId -> BoxPath -> ExtManager -> Model -> Html Msg


type alias NestingHitTest =
  BoxId -> BoxPath -> Point -> Maybe Id -> ExtManager -> Model -> Maybe Target


type alias NestingAutoSize =
  BoxPath -> ExtManager -> Model -> (Rectangle, Model)


-- TODO: wording, note: ExtManager is not passed
type alias NestingToolbar =
  BoxId -> Model -> ToolbarPos



-- VALUES


ext : ExtManager
ext =
  { view = view
  , hitTest = hitTest
  , autoSize = autoSize
  , toolbar = toolbar
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
        , toolbar = TopicMap.Geometry.toolbarPos
        }
      )
    , ("List",
        { name = "List"
        , label = "List"
        , view = TopicList.TopicList.view
        , hitTest = TopicList.Geometry.hitTest
        , autoSize = TopicList.Geometry.autoSize
        , toolbar = TopicList.Geometry.toolbarPos
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
    (\renderer -> renderer.view boxId boxPath ext model)


hitTest : BoxId -> BoxPath -> Point -> Maybe Id -> Model -> Maybe Target
hitTest boxId boxPath pos excludeTopicId model =
  dispatch boxId model Nothing
    (\renderer -> renderer.hitTest boxId boxPath pos excludeTopicId ext model)


autoSize : BoxPath -> Model -> (Rectangle, Model)
autoSize boxPath model =
  dispatch (Box.firstId boxPath) model (Rectangle 0 0 0 0, model)
    (\renderer -> renderer.autoSize boxPath ext model)


toolbar : BoxId -> Model -> ToolbarPos
toolbar boxId model =
  dispatch boxId model (ToolbarPos (\_ -> Point 0 0) (\_ -> Point 0 0))
    (\renderer -> renderer.toolbar boxId model)


dispatch : BoxId -> Model -> result -> (Extension -> result) -> result
dispatch boxId model errVal func =
  Box.rendererOf boxId model
    |> Maybe.andThen (\renderer -> registry |> Dict.get renderer)
    |> Maybe.map func
    |> Maybe.withDefault errVal
