port module AppEmbed exposing (Flags, Model, Msg, main)

import Browser
import Html exposing (Html, div)
import Main

type alias Model = Main.Model
type alias Msg   = Main.Msg
type alias Flags = Main.Flags

viewElement : Model -> Html Msg =
    \model -> let doc = Main.view model in div [] doc.body

main : Program Flags Model Msg
main =
    Browser.element
        { init = Main.init
        , update = Main.update
        , subscriptions = Main.subscriptions
        , view = viewElement
        }
