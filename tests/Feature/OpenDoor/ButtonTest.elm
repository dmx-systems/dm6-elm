module Feature.OpenDoor.ButtonTest exposing (tests)

import AppModel exposing (Model)
import Compat.ModelAPI as M exposing (createTopic, defaultModel, getMapItemById, isMapTopic)
import Dict
import Expect
import Feature.Cross as Cross
import Html
import Html.Attributes as Attr
import Main exposing (MainMsg(..), view)
import Model exposing (Id, Map, MapId, Point, Rectangle, Size)
import ModelAPI exposing (addItemToMap)
import Test exposing (..)
import Test.Html.Event as Event
import Test.Html.Query as Query
import Test.Html.Selector as Sel


{-| Build a model where:

  - there is a container with an inner map (parent = 0)
  - a child topic lives inside that container
  - we are currently viewing the container’s inner map
  - that child is selected in that inner map

-}
setupModel : ( Model, MapId, Id )
setupModel =
    let
        -- start with default model
        ( m1, containerId ) =
            createTopic "Container" Nothing defaultModel

        -- container visible on home map (0) as a container
        m2 =
            M.addItemToMapDefault containerId 0 m1

        -- give the container its inner map (parent = 0)
        m3 =
            { m2
                | maps =
                    Dict.insert containerId (Map containerId Dict.empty (Rectangle 0 0 0 0) 0) m2.maps
            }

        -- create a child topic inside the container
        ( m4, topicId ) =
            createTopic "Child" Nothing m3

        m5 =
            M.addItemToMapDefault topicId containerId m4

        -- We are *inside* the container and the child is selected there
        m6 =
            { m5
                | mapPath = containerId :: m5.mapPath
                , selection = [ ( topicId, containerId ) ]
            }
    in
    ( m6, containerId, topicId )


tests : Test
tests =
    describe "Toolbar Cross button"
        [ test "Clicking 'Cross' dispatches MoveTopicToParentMap with correct ids (and is enabled)" <|
            \_ ->
                let
                    ( model0, containerId, topicId ) =
                        setupModel

                    -- Render document body
                    root =
                        Html.div [] (view model0).body
                            |> Query.fromHtml

                    openDoorBtn =
                        root
                            |> Query.find [ Sel.id "btn-Cross" ]
                in
                Expect.all
                    [ -- 1) The button must be enabled (no 'disabled' attribute)
                      \btn -> Query.hasNot [ Sel.attribute (Attr.disabled True) ] btn

                    -- 2) Clicking it must dispatch the expected message
                    , \btn ->
                        btn
                            |> Event.simulate Event.click
                            |> Event.expect (CrossMsg Cross.CrossClick)
                    ]
                    openDoorBtn
        ]
