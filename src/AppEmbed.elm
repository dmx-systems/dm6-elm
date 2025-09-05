port module AppEmbed exposing (Flags, Model, Msg, main)

import Browser
import Html exposing (Html, div)
import Main



-- Optional outgoing ports (subscribe in JS only if present)


port store : String -> Cmd msg


port persist : String -> Cmd msg


type alias Model =
    Main.Model


type alias Msg =
    Main.Msg


type alias Flags =
    Main.Flags



-- { slug : String, stored : String }


viewElement : Model -> Html Msg
viewElement model =
    let
        doc =
            Main.view model
    in
    -- render only the body from Main.view inside our mount node
    div [] doc.body


main : Program Flags Model Msg
main =
    Browser.element
        { init = Main.init
        , update = Main.update
        , subscriptions = Main.subscriptions
        , view = viewElement
        }
