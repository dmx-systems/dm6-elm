module BoxRendererDef exposing (Renderer(..), all, encode, decoder, toName)

-- import ModelParts exposing (BoxId) -- TODO: creates circular dependency
-- import Utils as U -- TODO: creates circular dependency

import Dict exposing (Dict)
import Json.Decode as D
import Json.Encode as E



-- TYPES


type Renderer
  = TopicMap
  | List


type alias RendererInfo =
  { name : String
  , label : String
  , renderer : Renderer
  }



-- VALUES


-- key = renderer name
all : Dict String RendererInfo
all =
  Dict.fromList
    [ ("TopicMap", RendererInfo "TopicMap" "Topic Map" TopicMap)
    , ("List",     RendererInfo "List"     "List"      List)
    ]



-- JSON


encode : Renderer -> E.Value
encode =
  toName >> E.string


decoder : D.Decoder String -> D.Decoder Renderer
decoder =
  D.andThen
    (\name ->
      case byName name of
        Just renderer -> D.succeed renderer
        Nothing -> D.fail ("\"" ++ name ++ "\" is an invalid Renderer")
    )


--

toName : Renderer -> String
toName r =
  let
    rs = all
      |> Dict.values
      |> List.filter (\{renderer} -> renderer == r)
  in
  case rs of
    [{name}] -> name
    _ -> "" -- U.logError "BoxRendererDef.toName" "" ""


byName : String -> Maybe Renderer
byName name =
  case all |> Dict.get name of
    Just {renderer} -> Just renderer
    Nothing -> Nothing
