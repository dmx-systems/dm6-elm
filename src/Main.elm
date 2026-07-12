port module Main exposing (..)

import Box
import Config as C
import Env exposing (Env, Env2)
import ExtManager exposing (ext)
import Feature.Icon as Icon
import Feature.Mouse as Mouse
import Feature.MouseDef as MouseDef
import Feature.Nav as Nav
import Feature.Search as Search
import Feature.Sel as Sel
import Feature.Text as Text
import Feature.Tool as Tool
import Model exposing (Model, Msg(..))
import ModelBase exposing (..)
import Shared.Events as Events
import Storage as S
import TopicMap.Controller as TMC
import TopicMap.TopicMap as TopicMap
import Undo exposing (UndoModel)
import Utils as U

import Browser
import Html exposing (Html, div, text, br, a)
import Html.Attributes exposing (id, style, href)
import Json.Decode as D
import Json.Encode as E
import String exposing (fromInt, fromFloat)



-- PORTS


port onScroll : (Point -> msg) -> Sub msg



-- MAIN


main : Program (E.Value, String) UndoModel Msg
main =
  Browser.document
    { init = init
    , view = view
    , update = update
    , subscriptions =
        (\model -> Sub.batch
          [ Text.sub
          , Nav.sub
          , onScroll Scrolled
          ]
        )
    }


init : (E.Value, String) -> (UndoModel, Cmd Msg)
init (flags, hash) =
  let
    model = initModel flags
    boxId =
      case Nav.boxIdFromHash hash of
        Just boxId_ -> boxId_
        Nothing -> model.boxId
    cmd = Nav.pushUrl boxId
  in
  (model, cmd) |> Undo.reset


initModel : E.Value -> Model
initModel flags =
  case flags |> D.decodeValue (D.null True) of
    Ok True ->
      let
        _ = U.info "Main.initModel" "localStorage: empty"
      in
      Model.init
    _ ->
      case flags |> D.decodeValue Model.decoder of
        Ok model ->
          let
            _ = U.info "Main.initModel" ("localStorage: " ++ bytes ++ " bytes")
            bytes = model |> U.toString |> String.length |> fromInt
          in
          model
        Err e ->
          let
            _ = U.logError "Main.initModel" "localStorage" e
          in
          Model.init



-- VIEW


view : UndoModel -> Browser.Document Msg
view ({present} as undoModel) =
  Browser.Document
    "DM6 Elm"
    [ div
        ( Events.globalMouseHandler
          ++ appStyle
        )
        [ div
            headerStyle
            ( [ viewMapTitle present
              , viewSpacer
              ]
              ++ Tool.viewGlobalTools present
              ++ Search.viewSearchResult present -- TODO: move to "main" for scroll along?
            )
        , div
            ( [ id "main" ]
              ++ mainStyle
            )
            ( [ ext.view present.boxId [] present ] -- boxPath = []
              ++ Tool.viewMapTools undoModel
            )
        ]
    , viewFooter
    , viewMeasure present
    ]


appStyle : Attrs Msg
appStyle =
  [ style "display" "flex"
  , style "flex-direction" "column"
  , style "height" "100%"
  , style "font-family" C.mainFont
  , style "user-select" "none"
  , style "-webkit-user-select" "none" -- Safari still needs vendor prefix
  ]


headerStyle : Attrs Msg
headerStyle =
  [ style "display" "flex"
  , style "align-items" "center"
  , style "gap" "10px"
  , style "height" <| fromInt C.appHeaderHeight ++ "px"
  , style "padding" "0 10px"
  , style "background-color" C.toolbarColor
  ]


viewMapTitle : Model -> Html Msg
viewMapTitle model =
  div
    mapTitleStyle
    [ text <| Box.mapTitle model ]


mapTitleStyle : Attrs Msg
mapTitleStyle =
  [ style "font-size" "26px"
  , style "font-weight" "bold"
  , style "overflow" "hidden"
  , style "text-overflow" "ellipsis"
  , style "white-space" "nowrap"
  ]


viewSpacer : Html Msg
viewSpacer =
  div
  [ style "flex-grow" "1" ]
  []


mainStyle : Attrs Msg
mainStyle =
  [ style "position" "relative"
  , style "flex-grow" "1"
  , style "overflow" "auto"
  ]


viewFooter : Html Msg
viewFooter =
  div
    footerStyle
    [ div
        []
        [ text C.version ]
    , div
        []
        [ text C.date ]
    , div
        []
        [ text "Source: "
        , a
            ( [ href "https://github.com/dmx-systems/dm6-elm" ]
              ++ linkStyle
            )
            [ text "GitHub" ]
        ]
    , a
        ( [ href "https://dmx.berlin" ]
          ++ linkStyle
        )
        [ text "DMX Berlin" ]
    ]


footerStyle : Attrs Msg
footerStyle =
  [ style "font-family" C.mainFont
  , style "font-size" <| fromInt C.footerFontSize ++ "px"
  , style "position" "absolute"
  , style "bottom" "20px"
  , style "right" "20px"
  , style "text-align" "right"
  , style "color" "lightgray" -- #d3d3d3
  ]


linkStyle : Attrs Msg
linkStyle =
  [ style "color" "lightgray" ]


viewMeasure : Model -> Html Msg
viewMeasure model =
  div
    ( [ id "measure" ]
      ++ measureStyle
    )
    [ text model.text.measure
    , br [] []
    ]


measureStyle : Attrs Msg
measureStyle =
  [ style "position" "fixed"
  , style "visibility" "hidden"
  , style "white-space" "pre-wrap"
  , style "font-family" C.editorFont
  , style "font-size" <| fromInt C.contentFontSize ++ "px"
  , style "line-height" <| fromFloat C.topicLineHeight
  , style "padding" <| fromInt C.topicDetailPadding ++ "px"
  , style "width" <| fromInt C.topicDetailMaxWidth ++ "px"
  , style "min-width" <| fromInt (C.topicSize.w - C.topicSize.h) ++ "px"
  , style "max-width" "max-content"
  , style "border-width" <| fromInt C.topicBorderWidth ++ "px"
  , style "border-style" "solid"
  , style "box-sizing" "border-box"
  ]



-- UPDATE


update : Msg -> UndoModel -> (UndoModel, Cmd Msg)
update msg ({present} as undoModel) =
  let
    env =
      { model = present
      , undoModel = undoModel
      , ext = ext
      }
    _ =
      case msg of
        Mouse (MouseDef.Move _) -> msg
        _ -> U.info "Main.update" msg
  in
  case msg of
    -- renderer modules
    TopicMap topicMapMsg -> TMC.update topicMapMsg env
    -- feature modules
    Tool toolMsg -> Tool.update toolMsg env
    Text textMsg -> Text.update textMsg env
    Mouse mouseMsg -> Mouse.update mouseMsg env
    Search searchMsg -> Search.update searchMsg env
    Icon iconMenuMsg -> Icon.update iconMenuMsg undoModel
    Nav navMsg -> Nav.update navMsg env
    --
    Scrolled pos -> updateScrollPos pos present |> S.store |> Undo.swap undoModel
    Cancel maybeTarget -> cancelUI maybeTarget (Env.from env) |> Undo.swap undoModel
    NoOp -> (undoModel, Cmd.none)


cancelUI : Maybe Target -> Env2 -> (Model, Cmd Msg)
cancelUI maybeTarget env =
  env
    |> Env.map Icon.closePicker
    |> Env.map Search.closeMenu
    |> Env.map Tool.closeMenu
    |> cancelUIWith maybeTarget


cancelUIWith : Maybe Target -> Env2 -> (Model, Cmd Msg)
cancelUIWith maybeTarget ({model} as env) =
  let
    isTargeted =
      case maybeTarget of
        Just (T topicId as itemId, boxPath) ->
          Sel.isSelected itemId boxPath model ||
          Mouse.isHovered topicId boxPath model
        Just (A _ as itemId, boxPath) ->
          Sel.isSelected itemId boxPath model
        _ -> False
  in
  if isTargeted then
    (model, Cmd.none) -- keep selection, hover state, and edit mode
  else
    env
      |> Env.map Sel.clear
      |> Env.map Mouse.clearHover -- TODO: needed?
      |> Text.leaveEdit


updateScrollPos : Point -> Model -> Model
updateScrollPos pos model =
  TopicMap.updateScrollPos model.boxId (\_ -> pos) model
