module Extensions exposing
    ( ActionDescriptor
    , Msg(..)
    , dispatcher
    )

import Dict exposing (Dict)



-- A single bus message carrying an action id


type Msg msg
    = Run String



-- Describe an action without coupling to your app


type alias ActionDescriptor model msg =
    { id : String
    , label : String
    , icon : Maybe String
    , enabled : model -> Bool
    , run : model -> ( model, Cmd msg )
    }



-- Given a registry, run the action by id


dispatcher :
    Dict String (ActionDescriptor model msg)
    -> Msg msg
    -> model
    -> ( model, Cmd msg )
dispatcher registry message model =
    case message of
        Run id ->
            case Dict.get id registry of
                Just d ->
                    d.run model

                Nothing ->
                    ( model, Cmd.none )
