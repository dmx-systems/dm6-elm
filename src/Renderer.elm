module Renderer exposing (Renderer(..), default, toString, fromString, encode, decoder)

-- import Console -- TODO: cyclic

import Json.Decode as D
import Json.Encode as E



type Renderer
  = TopicMap
  | TopicList


default : Renderer
default =
  TopicMap


toString : Renderer -> String
toString renderer =
  case renderer of
    TopicMap -> "TopicMap"
    TopicList -> "TopicList"


fromString : String -> Maybe Renderer
fromString str =
  case str of
    "TopicMap" -> Just TopicMap
    "TopicList" -> Just TopicList
    _ ->
      -- let
      --   _ = Console.logError "Renderer.fromString" "Unknown renderer" str
      -- in
      Nothing



-- JSON


encode : Renderer -> E.Value
encode =
  toString >> E.string


decoder : D.Decoder String -> D.Decoder Renderer
decoder strDecoder =
  strDecoder |> D.andThen
    (\str ->
      case fromString str of
        Just renderer -> D.succeed renderer
        Nothing -> D.fail ("Unknown renderer: \"" ++ str ++ "\"")
    )
