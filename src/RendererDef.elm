module RendererDef exposing (Renderer(..), encode, decoder, toString)

import Json.Decode as D
import Json.Encode as E



-- TYPES


type Renderer
  = TopicMap
  | List



-- JSON


encode : Renderer -> E.Value
encode =
  toString >> E.string


decoder : D.Decoder String -> D.Decoder Renderer
decoder =
  D.andThen
    (\str ->
      case fromString str of
        Just renderer -> D.succeed renderer
        Nothing -> D.fail ("\"" ++ str ++ "\" is an invalid Renderer")
    )


--

toString : Renderer -> String
toString renderer =
  case renderer of
    TopicMap -> "TopicMap"
    List -> "List"


fromString : String -> Maybe Renderer
fromString str =
  case str of
    "TopicMap" -> Just TopicMap
    "List" -> Just List
    _ -> Nothing
