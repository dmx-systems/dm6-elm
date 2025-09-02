module Main exposing (init, main, subscriptions, update, view)

import AppModel as AM
import Browser exposing (Document)
import Domain.Reparent as R
import Html exposing (Html, text)
import Model exposing (MapId)



-- Aliases so we can write Html Msg / Program () Model Msg cleanly


type alias Model =
    AM.Model


type alias Msg =
    AM.Msg



-- PROGRAM WIRING


init : () -> ( Model, Cmd Msg )
init _ =
    ( AM.default, Cmd.none )


update : Msg -> Model -> ( Model, Cmd Msg )
update _ model =
    -- TODO: replace with your real update
    ( model, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions _ =
    -- TODO: replace with your real subscriptions
    Sub.none


view : Model -> Document Msg
view _ =
    -- TODO: replace with your real view
    { title = "dm6-elm"
    , body = [ text "dm6-elm" ]
    }


main : Program () Model Msg
main =
    Browser.document
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }



-- OPEN-DOOR / REPARENT ADAPTER (pure invariant lives in Domain.Reparent)


canReparent : MapId -> Maybe MapId -> AM.Model -> Result String ()
canReparent a b model =
    R.canReparent a b (parentsOf model)



-- Curried so (parentsOf model) : MapId -> List MapId


parentsOf : AM.Model -> MapId -> List MapId
parentsOf model childId =
    -- TODO: derive direct parent map IDs of `childId` from your dmx.composition assocs
    []
