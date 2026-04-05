module Renderer exposing (view)

import Box
import Model exposing (Model, Msg(..))
import ModelParts exposing (BoxId, BoxPath)
import RendererDef exposing (toName)
-- renderer modules
import TopicMap.View

import Dict exposing (Dict)
import Html exposing (Html, text)



-- TYPES


type alias RendererFunc =
  { view : View
  }


type alias View = BoxId -> BoxPath -> Model -> Html Msg



-- VALUES


-- key = renderer name
render : Dict String RendererFunc
render =
  Dict.fromList
    [ ("TopicMap", RendererFunc TopicMap.View.view)
    ]



-- VIEW


view : BoxId -> BoxPath -> Model -> Html Msg
view boxId boxPath model =
  case Box.rendererOf boxId model of
    Just renderer ->
      case Dict.get (toName renderer) render of
        Just renderFunc -> renderFunc.view boxId boxPath model
        Nothing -> text "Renderer ??"
    Nothing -> text "Renderer ??"
