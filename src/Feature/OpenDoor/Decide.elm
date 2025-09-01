-- Feature/OpenDoor/Decide.elm


module Feature.OpenDoor.Decide exposing (decideOpenDoorMsg)

import AppModel exposing (Model, Msg(..))
import Model exposing (..)
import ModelAPI exposing (getSingleSelection)


{-| Decide what the Cross button should do in the current state.
For the current tests: with exactly one selected topic,
emit a MoveTopicToMap with same src/dst map and zero offsets.
-}
decideOpenDoorMsg : Model -> Maybe Msg
decideOpenDoorMsg model =
    case getSingleSelection model of
        Just ( topicId, mapId ) ->
            let
                origin =
                    Point 0 0
            in
            Just (MoveTopicToMap topicId mapId origin topicId mapId origin)

        Nothing ->
            Nothing
