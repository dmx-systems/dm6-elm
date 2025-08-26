module Navigation.AdjustMapRectRegressionTest exposing (tests)

import AppModel exposing (Model)
import Compat.ModelAPI as M exposing (addItemToMapDefault, createTopic, defaultModel, getMapItemById, isMapTopic)
import Model exposing (Id, Map, MapId, Point, Rectangle, Size)
import ModelAPI exposing (addItemToMap)
import Test exposing (..)


tests : Test
tests =
    describe "adjustMapRect regression: Back after Fullscreen must not create negative sizes"
        [ test "Back after Fullscreen keeps non-negative rect width/height" <|
            \_ ->
                let
                    -- 1) start with a container topic on the home map (0)
                    ( m1, cId ) =
                        createTopic "Container" Nothing defaultModel

                    m2 =
                        M.addItemToMapDefault cId 0 defaultModel

                    -- 2) select it so Main.update can react to Nav.Fullscreen
                    m3 =
                        select cId 0 m2

                    -- 3) go Fullscreen (this pushes cId onto mapPath and pans its map)
                    mFs =
                        update (Nav Fullscreen) m3 |> Tuple.first

                    -- 4) simulate that the container's inner map has content/size now
                    --    (non-zero width/height). This is what makes the old buggy
                    --    adjustMapRect produce negative width on Back.
                    mSized =
                        updateMapRect cId (\_ -> Rectangle 0 0 200 200) mFs

                    -- 5) go Back (this calls adjustMapRect with factor=+1 on cId)
                    mBack =
                        update (Nav Back) mSized |> Tuple.first

                    rect =
                        case getMap cId mBack.maps of
                            Just m ->
                                m.rect

                            Nothing ->
                                Rectangle 0 0 0 0

                    width =
                        rect.x2 - rect.x1

                    height =
                        rect.y2 - rect.y1
                in
                -- With the old (buggy) implementation (shift only x1/y1), width == -200 here.
                -- With the fixed implementation (shift both corners), width == 200 (OK).
                Expect.equal True (width >= 0 && height >= 0)
        ]
