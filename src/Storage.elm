port module Storage exposing (modelDecoder, storeModel, storeModelWith)

import AppModel as AM
import Defaults as Def
import Dict exposing (Dict)
import Json.Decode as D
import Json.Decode.Pipeline exposing (hardcoded, optional)
import Json.Encode as E
import Model exposing (..)
import Platform.Cmd as Cmd



-- PORTS


port store : E.Value -> Cmd msg



-- ENCODING
-- (Minimal: we do not rely on reading this back; on load we decode {} with defaults.)


encodeModel : AM.Model -> E.Value
encodeModel _ =
    -- Intentionally minimal; runtime rehydrates transients via Defaults
    -- and persistent fields have optional fallbacks if missing.
    E.object []



-- DECODING
-- Make *persistent* fields optional with sensible defaults so `{}` decodes.


modelDecoder : D.Decoder AM.Model
modelDecoder =
    let
        mkModel items maps mapPath nextId selection editState measureText mouse search iconMenu =
            { items = items
            , maps = maps
            , mapPath = mapPath
            , nextId = nextId
            , selection = selection
            , editState = editState
            , measureText = measureText
            , mouse = mouse
            , search = search
            , iconMenu = iconMenu
            }

        -- Default "home" map (id 0, empty items, zero rect, parent = -1)
        defaultMaps : Dict MapId Map
        defaultMaps =
            Dict.singleton 0 (Map 0 Dict.empty (Rectangle 0 0 0 0) -1)

        -- If you already have rich decoders, replace these two with yours.
        itemsDecoder : D.Decoder (Dict Id Item)
        itemsDecoder =
            D.succeed Dict.empty

        mapsDecoder : D.Decoder (Dict MapId Map)
        mapsDecoder =
            D.succeed defaultMaps
    in
    D.succeed mkModel
        -- persistent
        |> optional "items" itemsDecoder Dict.empty
        |> optional "maps" mapsDecoder defaultMaps
        |> optional "mapPath" (D.list D.int) [ 0 ]
        |> optional "nextId" D.int 1
        -- transients (never stored; always reconstructed)
        |> hardcoded Def.selection
        |> hardcoded Def.editState
        |> hardcoded Def.measureText
        |> hardcoded Def.mouse
        |> hardcoded Def.search
        |> hardcoded Def.iconMenu



-- HELPERS TO STORE AFTER UPDATE


storeModel : AM.Model -> ( AM.Model, Cmd AM.Msg )
storeModel model =
    ( model, store (encodeModel model) )


storeModelWith : ( AM.Model, Cmd AM.Msg ) -> ( AM.Model, Cmd AM.Msg )
storeModelWith ( model, cmd ) =
    ( model, Cmd.batch [ cmd, store (encodeModel model) ] )
