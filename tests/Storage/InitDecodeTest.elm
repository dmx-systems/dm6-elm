module Storage.InitDecodeTest exposing (tests)

import Expect
import Json.Decode as Decode
import Storage exposing (modelDecoder)
import Test exposing (..)


tests : Test
tests =
    describe "Storage.modelDecoder init paths"
        [ test "new list blob -> decode" <|
            \_ ->
                let
                    json =
                        -- New canonical schema:
                        -- - maps: List
                        -- - topicProps.display (not displayMode)
                        -- - requires nextId and mapPath
                        """
                        {
                          "items": [
                            { "topic": { "id": 1, "text": "Hello", "icon": "" } },
                            { "assoc": { "id": 2, "type": "dmx.composition",
                                         "role1": "dmx.child", "player1": 1,
                                         "role2": "dmx.parent", "player2": 0 } }
                          ],
                          "maps": [
                            {
                              "id": 0,
                              "rect": { "x1": 0, "y1": 0, "x2": 0, "y2": 0 },
                              "items": [
                                {
                                  "id": 1,
                                  "parentAssocId": 2,
                                  "hidden": false,
                                  "pinned": false,
                                  "topicProps": {
                                    "pos":   { "x": 0, "y": 0 },
                                    "size":  { "w": 100, "h": 30 },
                                    "display": "LabelOnly"
                                  }
                                }
                              ]
                            }
                          ],
                          "mapPath": [0],
                          "nextId": 3
                        }
                        """
                in
                case Decode.decodeString modelDecoder json of
                    Ok _ ->
                        Expect.pass

                    Err e ->
                        Expect.fail <| "expected Ok, got Err: " ++ Decode.errorToString e
        , test "legacy dict blob -> decode" <|
            \_ ->
                let
                    legacyDict =
                        """
                {
                  "items": {
                    "1": { "topic": { "id": 1, "text": "Hello", "iconName": "" } },
                    "2": { "assoc": { "id": 2, "itemType": "dmx.composition",
                                      "player1": 1, "role1": "dmx.child",
                                      "player2": 0, "role2": "dmx.parent" } }
                  },
                  "maps": {
                    "0": {
                      "id": 0,
                      "items": {
                        "1": {
                          "id": 1,
                          "hidden": false,
                          "pinned": false,
                          "topicProps": {
                            "pos": { "x": 0, "y": 0 },
                            "size": { "w": 100, "h": 30 },
                            "displayMode": "LabelOnly"
                          },
                          "parentAssocId": 2
                        }
                      },
                      "rect": { "x1": 0, "y1": 0, "x2": 0, "y2": 0 }
                    }
                  },
                  "mapPath": [0],
                  "nextId": 3
                }
                """
                in
                case Decode.decodeString modelDecoder legacyDict of
                    Ok _ ->
                        Expect.pass

                    Err e ->
                        Expect.fail <| "expected legacy dict blob to decode, got Err: " ++ Decode.errorToString e
        , test "{} -> home map" <|
            \_ ->
                case Decode.decodeString modelDecoder "{}" of
                    Ok _ ->
                        Expect.pass

                    Err e ->
                        Expect.fail <| "Expected {} to decode to default model. Got: " ++ Decode.errorToString e
        ]
