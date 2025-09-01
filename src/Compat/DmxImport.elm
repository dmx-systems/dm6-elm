module Compat.DmxImport exposing (decodeCoreTopicToCore)

import Compat.CoreModel as Core
import Dict
import Json.Decode as D exposing (Decoder)
import Json.Decode.Pipeline exposing (optional, required)
import Model exposing (..)



-- Minimal shape for first cut


type alias CoreTopic =
    { id : Int
    , typeUri : String
    , value : String
    , children : Maybe D.Value
    , assocChildren : Maybe D.Value
    }


coreTopicDecoder : Decoder CoreTopic
coreTopicDecoder =
    D.succeed CoreTopic
        |> required "id" D.int
        |> required "typeUri" D.string
        |> required "value" D.string
        |> optional "children" (D.nullable D.value) Nothing
        |> optional "assocChildren" (D.nullable D.value) Nothing


decodeCoreTopicToCore : D.Value -> Result D.Error Core.CoreModel
decodeCoreTopicToCore v =
    D.decodeValue coreTopicDecoder v
        |> Result.map coreTopicToCore



-- First cut: root topic on home map (id=0)


coreTopicToCore : CoreTopic -> Core.CoreModel
coreTopicToCore t =
    let
        topicItem : Item
        topicItem =
            Item t.id (Topic (TopicInfo t.id t.value Nothing))

        mapItem : MapItem
        mapItem =
            { id = t.id
            , parentAssocId = 0
            , hidden = False
            , pinned = False
            , props =
                MapTopic
                    { pos = Point 0 0
                    , size = Size 156 28
                    , displayMode = Monad LabelOnly
                    }
            }

        home : Map
        home =
            Map 0 (Rectangle 0 0 0 0) (Dict.fromList [ ( t.id, mapItem ) ])
    in
    { items = Dict.fromList [ ( t.id, topicItem ) ]
    , maps = Dict.fromList [ ( 0, home ) ]
    , mapPath = [ 0 ]
    , nextId = t.id + 1
    }
