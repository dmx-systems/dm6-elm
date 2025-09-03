module Feature.OpenDoor.ButtonTest exposing (tests)

import AppModel as AM exposing (Msg(..), default)
import Dict
import Expect
import Html
import Html.Attributes as Attr
import Main exposing (view)
import Model exposing (Id, MapId, MapPath, Point, Size)
import ModelAPI exposing (..)
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
setupModel : ( AM.Model, MapId, Id )
setupModel =
    let
        -- start with default model
        ( m1, containerId ) =
            createTopic "Container" Nothing default

        -- container visible on home map (0) as a container
        m2 =
            addItemToMap containerId
                (Model.MapTopic (Model.TopicProps (Point 100 100) (Size 160 60) (Model.Container Model.BlackBox)))
                0
                m1

        -- give the container its inner map (parent = 0)
        m3 =
            { m2
                | maps =
                    Dict.insert containerId (Model.Map containerId (Model.Rectangle 0 0 0 0) Dict.empty) m2.maps
            }

        -- create a child topic inside the container
        ( m4, topicId ) =
            createTopic "Child" Nothing m3

        m5 =
            addItemToMap topicId
                (Model.MapTopic (Model.TopicProps (Point 30 30) (Size 120 40) (Model.Monad Model.LabelOnly)))
                containerId
                m4

        -- We are *inside* the container and the child is selected there
        m6 =
            let
                path : MapPath
                path =
                    containerId :: m5.mapPath
            in
            { m5 | mapPath = path }
                |> ModelAPI.select topicId path
    in
    ( m6, containerId, topicId )


tests : Test
tests =
    describe "Toolbar Cross button"
        [ test "Clicking 'Cross' dispatches MoveTopicToMap with correct ids (and is enabled)" <|
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

                    -- 2) Clicking it must dispatch the expected 6-arg message
                    , \btn ->
                        let
                            origin : Point
                            origin =
                                { x = 0, y = 0 }
                        in
                        btn
                            |> Event.simulate Event.click
                            |> Event.expect
                                (MoveTopicToMap
                                    topicId
                                    containerId
                                    origin
                                    topicId
                                    model0.mapPath
                                    origin
                                )
                    ]
                    openDoorBtn
        ]
