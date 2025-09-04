module Feature.OpenDoor.MoveTopicIntoContainerTest exposing (tests)

import AppModel as AM
import Compat.ModelAPI as ModelAPI exposing (addItemToMap, createAssoc, defaultProps)
import Expect
import Json.Decode as D
import Main
import Model exposing (..)
import Storage exposing (modelDecoder)
import Test exposing (..)


emptyRootModel : AM.Model
emptyRootModel =
    let
        json =
            """
            {
              "nextId": 1,
              "items": {},
              "maps": {
                "0": { "id": 0, "items": {} }
              }
            }
            """
    in
    case D.decodeString modelDecoder json of
        Ok m ->
            m

        Err err ->
            Debug.todo ("Bad emptyRootModel fixture: " ++ D.errorToString err)


tests : Test
tests =
    describe "OpenDoor: moving a topic INTO another topic"
        [ test "dragging X into B places X in B's container map (id = B)" <|
            \_ ->
                -- Arrange: start with empty default model and put B and X into root map (0)
                let
                    model0 =
                        -- default model has maps 0 (root) (and maybe 1), but no topics yet
                        emptyRootModel

                    -- create B and place it in root (0)
                    ( model1, bId ) =
                        ModelAPI.createTopic "B" Nothing model0

                    size =
                        Size 120 40

                    propsB =
                        Model.MapTopic (defaultProps bId size model1)

                    model2 =
                        ModelAPI.addItemToMap bId propsB 0 model1

                    -- create X and place it in root (0)
                    ( model3, xId ) =
                        ModelAPI.createTopic "X" Nothing model2

                    propsX =
                        Model.MapTopic (defaultProps xId size model3)

                    model4 =
                        ModelAPI.addItemToMap xId propsX 0 model3

                    -- Act: drag X onto B (targetId = bId) while we are viewing root [0]
                    -- Buggy implementation chooses destMapId from targetMapPath's head (0),
                    -- so X stays in root instead of entering B.
                    modelAfterMove =
                        Main.moveTopicToMap
                            xId
                            0
                            (Point 0 0)
                            bId
                            [ 0 ]
                            -- we are on root map when dropping onto B
                            (Point 30 30)
                            model4

                    xInB =
                        ModelAPI.isItemInMap xId bId modelAfterMove

                    xInRoot =
                        ModelAPI.isItemInMap xId 0 modelAfterMove
                in
                -- Expected: X was moved into B (present in map bId) and not left in root.
                -- Current buggy code will fail this by leaving xInB == False and xInRoot == True.
                Expect.all
                    [ \_ -> Expect.equal True xInB
                    , \_ -> Expect.equal False xInRoot
                    ]
                    ()
        ]
