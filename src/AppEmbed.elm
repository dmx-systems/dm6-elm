port module AppEmbed exposing (Flags, Model, Msg, main)

import AppModel as AM
import Browser
import Html exposing (Html, div)
import Main
import Platform.Sub as Sub



-- incoming page JSON from the frame


port pageJson : (String -> msg) -> Sub msg



-- Optional outgoing ports (okay to keep even if unused)


port store : String -> Cmd msg


port persist : String -> Cmd msg


type alias Model =
    Main.Model


type alias Msg =
    Main.Msg


type alias Flags =
    Main.Flags


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
        , subscriptions =
            \m ->
                Sub.batch
                    [ Main.subscriptions m
                    , pageJson AM.FedWikiPage -- <— wire port → message
                    ]
        , view = viewElement
        }
