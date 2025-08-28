port module Storage exposing (modelDecoder, storeModel, storeModelWith)

import AppModel as AM
import Config exposing (homeMapName)
import Defaults as Def
import Dict
import FedWiki exposing (encodePage, modelToPage, pageDecoder, pageToModel)
import Json.Decode as D
import Json.Decode.Pipeline exposing (hardcoded, optional)
import Json.Encode as E
import Model exposing (..)
import Platform.Cmd as Cmd


port store : E.Value -> Cmd msg



-- Keep your existing legacy decoder (the one that can handle {} / old blobs)


legacyDecoder : D.Decoder AM.Model
legacyDecoder =
    let
        defaultMaps =
            Dict.singleton 0 (Map 0 Dict.empty (Rectangle 0 0 0 0) -1)
    in
    D.succeed
        (\items maps mapPath nextId ->
            { items = items
            , maps = maps
            , mapPath = mapPath
            , nextId = nextId
            , selection = Def.selection
            , editState = Def.editState
            , measureText = Def.measureText
            , mouse = Def.mouse
            , search = Def.search
            , iconMenu = Def.iconMenu
            , journal = []
            }
        )
        |> optional "items" (D.succeed Dict.empty) Dict.empty
        |> optional "maps" (D.succeed defaultMaps) defaultMaps
        |> optional "mapPath" (D.list D.int) [ 0 ]
        |> optional "nextId" D.int 1



-- NEW: prefer FedWiki pages, fall back to legacy


modelDecoder : D.Decoder AM.Model
modelDecoder =
    D.oneOf
        [ pageDecoder |> D.map pageToModel
        , legacyDecoder
        ]


storeModel : AM.Model -> ( AM.Model, Cmd AM.Msg )
storeModel model =
    let
        page =
            modelToPage homeMapName model.journal model
    in
    ( model, store (encodePage page) )


storeModelWith : ( AM.Model, Cmd AM.Msg ) -> ( AM.Model, Cmd AM.Msg )
storeModelWith ( model, cmd ) =
    let
        page =
            modelToPage homeMapName model.journal model
    in
    ( model, Cmd.batch [ cmd, store (encodePage page) ] )
