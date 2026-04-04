module RendererDef exposing (Renderer(..), encode, decoder)

import Json.Decode as D
import Json.Encode as E



-- TYPES


type Renderer
  = TopicMap
  | List



-- JSON


encode : Renderer -> E.Value
encode renderer =
  E.string <|
    case renderer of
      TopicMap -> "TopicMap"
      List -> "List"


decoder : D.Decoder Renderer
decoder =
  D.string |> D.andThen
    (\str ->
      case str of
        "TopicMap" -> D.succeed TopicMap
        "List" -> D.succeed List
        _ -> D.fail ("\"" ++ str ++ "\" is an invalid Renderer")
    )
