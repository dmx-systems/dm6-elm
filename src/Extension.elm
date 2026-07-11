module Extension exposing (Renderer(..), defaultRenderer, toString, fromString, encodeRenderer,
  rendererDecoder)

-- import Utils as U -- TODO: cyclic

import Json.Decode as D
import Json.Encode as E



type Renderer
  = TopicMap
  | TopicList


defaultRenderer : Renderer
defaultRenderer =
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
      --   _ = U.logError "Extension.fromString" "Unknown renderer" str
      -- in
      Nothing



-- JSON


encodeRenderer : Renderer -> E.Value
encodeRenderer =
  toString >> E.string


rendererDecoder : D.Decoder String -> D.Decoder Renderer
rendererDecoder strDecoder =
  strDecoder |> D.andThen
    (\str ->
      case fromString str of
        Just renderer -> D.succeed renderer
        Nothing -> D.fail ("Unknown renderer: \"" ++ str ++ "\"")
    )
