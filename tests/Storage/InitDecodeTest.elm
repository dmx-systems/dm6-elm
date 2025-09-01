module Storage.InitDecodeTest exposing (tests)

import AppModel exposing (Model)
import Dict exposing (Dict)
import Expect
import Json.Decode as D
import Storage exposing (modelDecoder)
import Test exposing (..)


tests : Test
tests =
    describe "Storage.modelDecoder init paths"
        [ test "{} -> home map" <|
            \_ ->
                case D.decodeString modelDecoder "{}" of
                    Ok model ->
                        Expect.equal True (Dict.member 0 model.maps)
                            |> Expect.onFail "home map (id 0) should exist"

                    Err err ->
                        Expect.fail ("Expected {} to decode, but got: " ++ D.errorToString err)
        , test "legacy dict blob -> decode" <|
            \_ ->
                case D.decodeString modelDecoder legacyDictJson of
                    Ok model ->
                        case Dict.get 0 model.maps of
                            Just home ->
                                let
                                    hasItem1 =
                                        Dict.member 1 home.items
                                in
                                Expect.equal True hasItem1
                                    |> Expect.onFail "home map exists and contains item id=1"

                            Nothing ->
                                Expect.fail "home map (id 0) missing"

                    Err err ->
                        Expect.fail ("Expected legacy dict blob to decode, but got: " ++ D.errorToString err)
        , test "new list blob -> decode" <|
            \_ ->
                case D.decodeString modelDecoder listJson of
                    Ok model ->
                        case Dict.get 0 model.maps of
                            Just home ->
                                let
                                    hasItem1 =
                                        Dict.member 1 home.items
                                in
                                Expect.equal True hasItem1
                                    |> Expect.onFail "home map exists and contains item id=1"

                            Nothing ->
                                Expect.fail "home map (id 0) missing"

                    Err err ->
                        Expect.fail ("Expected list blob to decode, but got: " ++ D.errorToString err)
        ]



-- A minimal “legacy” dict-shaped JSON (string keys for Dicts)


legacyDictJson : String
legacyDictJson =
    """
    {
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
              "parentAssocId": 0
            }
          },
          "rect": { "x1": 0, "y1": 0, "x2": 0, "y2": 0 }
        }
      },
      "mapPath": [0],
      "nextId": 2
    }
    """



-- A minimal “new” list-shaped JSON (lists for items/maps)


listJson : String
listJson =
    """
    {
      "items": [
        { "topic": { "id": 1, "text": "Hello", "iconName": "" } }
      ],
      "maps": [
        {
          "id": 0,
          "items": [
            {
              "id": 1,
              "hidden": false,
              "pinned": false,
              "topicProps": {
                "pos": { "x": 0, "y": 0 },
                "size": { "w": 100, "h": 30 },
                "displayMode": "LabelOnly"
              },
              "parentAssocId": 0
            }
          ],
          "rect": { "x1": 0, "y1": 0, "x2": 0, "y2": 0 }
        }
      ],
      "mapPath": [0],
      "nextId": 2
    }
    """
