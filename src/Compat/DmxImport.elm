-- Compat/DmxImport.elm


module Compat.DmxImport exposing (decodeCoreTopicToModel)

import Dict exposing (Dict)
import Json.Decode as D exposing (Decoder)
import Json.Decode.Pipeline exposing (optional, required)
import Model exposing (..)


type alias CoreTopic =
    { id : Int
    , typeUri : String
    , value : String
    , children : D.Value -- shape varies by type; keep as Value for now
    , assocChildren : D.Value -- likewise
    }


coreTopicDecoder : Decoder CoreTopic
coreTopicDecoder =
    D.succeed CoreTopic
        |> required "id" D.int
        |> required "typeUri" D.string
        |> required "value" D.string
        |> optional "children" D.value D.null
        |> optional "assocChildren" D.value D.null


decodeCoreTopicToModel : D.Value -> Result D.Error Model
decodeCoreTopicToModel v =
    case D.decodeValue coreTopicDecoder v of
        Ok t ->
            Ok (coreTopicToDm6 t)

        Err e ->
            Err e


coreTopicToDm6 : CoreTopic -> Model
coreTopicToDm6 t =
    -- Minimal: single map with the root topic.
    let
        topicItem =
            Item t.id (Topic (TopicInfo t.id t.value Nothing))

        map0 =
            Map 0
                -1
                (Rectangle 0 0 0 0)
                (Dict.fromList
                    [ ( t.id
                      , { id = t.id
                        , parentAssocId = 0
                        , hidden = False
                        , pinned = False
                        , props = MapTopic { pos = Point 0 0, size = Size 156 28, displayMode = Monad LabelOnly }
                        }
                      )
                    ]
                )
    in
    { items = Dict.fromList [ ( t.id, topicItem ) ]
    , maps = Dict.fromList [ ( 0, map0 ) ]
    , mapPath = [ 0 ]
    , nextId = t.id + 1
    , selection = []
    , editState = NoEdit
    , measureText = ""
    , mouse = Mouse.default
    , search = Search.default
    , iconMenu = IconMenu.default
    }
