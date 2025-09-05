module Main exposing (Flags, Model, Msg, init, main, moveTopicToMap, subscriptions, update, view)

import AppModel as AM
import Browser
import Feature.Connection.Channel as Channel
import FedWiki
import Html exposing (Html, div, h4, pre, text)
import Html.Attributes exposing (style)
import Json.Decode as D
import Model exposing (Id, Point)
import Storage exposing (modelDecoder)
import String



-- Reuse canonical types


type alias Model =
    AM.Model


type alias Msg =
    AM.Msg



-- Flags from JS bootstrap


type alias Flags =
    { slug : String
    , stored : String
    }



-- Lenient loader: try Storage.modelDecoder, else fall back to AM.default


decodeOrDefault : String -> Model
decodeOrDefault raw =
    case D.decodeString modelDecoder raw of
        Ok m ->
            m

        Err _ ->
            AM.default



-- INIT


init : Flags -> ( Model, Cmd Msg )
init flags =
    let
        m0 =
            decodeOrDefault flags.stored
    in
    ( m0, Cmd.none )



-- UPDATE


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        AM.FedWikiPage raw ->
            ( model
                |> FedWiki.renderAsMonad raw
                -- ensure a monad exists
                |> (\m -> { m | fedWikiRaw = raw })
              -- keep raw for the panel
            , Cmd.none
            )

        _ ->
            ( model, Cmd.none )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none



-- VIEW


view : Model -> Browser.Document Msg
view model =
    { title = "DM6 Elm"
    , body =
        [ -- your existing UI goes here (toolbar, map, etc.)
          -- For now, keep it minimal and add a tiny viewer at the bottom:
          div [ style "padding" "8px" ]
            [ h4 [ style "margin" "0 0 6px 0", style "font-family" "system-ui" ] [ text "FedWiki page JSON" ]
            , if String.isEmpty model.fedWikiRaw then
                div [ style "color" "#666", style "font-family" "system-ui", style "font-size" "12px" ]
                    [ text "No page JSON received yet." ]

              else
                pre
                    [ style "white-space" "pre-wrap"
                    , style "word-break" "break-word"
                    , style "font-family" "ui-monospace, SFMono-Regular, Menlo, Consolas, monospace"
                    , style "font-size" "12px"
                    , style "border" "1px solid #eee"
                    , style "padding" "8px"
                    , style "border-radius" "6px"
                    , style "background" "#fafafa"
                    ]
                    [ text model.fedWikiRaw ]
            ]
        ]
    }



-- MAIN


main : Program Flags Model Msg
main =
    Browser.document
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }


{-| Compatibility wrapper used by Feature.OpenDoor.\* and tests.

    moveTopicToMap topicId fromId origPos targetId parentPath pos model

-}
moveTopicToMap :
    Id
    -> Id
    -> Point
    -> Id
    -> List Id
    -> Point
    -> Model
    -> Model
moveTopicToMap topicId fromId origPos targetId parentPath pos model =
    let
        fromBoundary =
            if fromId == 0 then
                Channel.Root

            else
                Channel.Container fromId

        toBoundary =
            if targetId == 0 then
                Channel.Root

            else
                Channel.Container targetId

        req =
            { topicId = topicId
            , from = fromBoundary
            , to = toBoundary
            , pos = pos
            , permit = Channel.defaultPermit
            }
    in
    case Channel.cross req model of
        Ok ( model1, _, _ ) ->
            model1

        Err _ ->
            model
