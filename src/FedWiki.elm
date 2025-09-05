module FedWiki exposing (renderAsMonad)

import AppModel as AM
import Json.Decode as D
import ModelAPI exposing (createTopicIn)



-- optional: import MapAutoSize exposing (autoSize)
-- decode just the FedWiki page title


titleDecoder : D.Decoder String
titleDecoder =
    D.field "title" D.string


{-| Given raw FedWiki page JSON, create a Topic with that title
on the home map [0]. If decoding fails, leave model unchanged.
-}
renderAsMonad : String -> AM.Model -> AM.Model
renderAsMonad raw model =
    case D.decodeString titleDecoder raw of
        Ok title ->
            -- createTopicIn : String -> Maybe String -> List MapId -> Model -> Model
            createTopicIn title Nothing [ 0 ] model

        -- |> autoSize   -- uncomment if you want immediate layout
        Err _ ->
            model
