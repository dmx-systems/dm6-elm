port module Main exposing (..)

import Box
import Config as C
import Console
import Dict
import Dispatch exposing (dispatch)
import Env exposing (Env)
import Feature.Crdt as Crdt
import Feature.Icon as Icon
import Feature.Id as Id
import Feature.Mouse as Mouse
import Feature.MouseDef as MouseDef
import Feature.Nav as Nav
import Feature.Search as Search
import Feature.Sel as Sel
import Feature.Text as Text
import Feature.Tool as Tool
import Model exposing (Model, Msg(..))
import ModelBase exposing (..)
import Outcome exposing (..)
import Renderer
import Shared.Events as Events
import TopicMap.Controller as TMC
import TopicMap.TopicMap as TopicMap

import Browser
import Html exposing (Html, div, text, br)
import Html.Attributes exposing (id, style)
import Json.Decode as D
import Json.Encode as E
import String exposing (fromInt, fromFloat)



-- PORTS


port domReady : () -> Cmd msg

port onScroll : (Point -> msg) -> Sub msg



-- MAIN


type AppModel
  = Bootstrapping
  | Running Model


main : Program (E.Value, E.Value, String) AppModel Msg
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


init : (E.Value, E.Value, String) -> (AppModel, Cmd Msg)
init (flags, ops, hash) =
  let
    appModel = initModel flags ops
    initCmd =
      case appModel of
        Bootstrapping ->
          Id.generateIds C.idPoolSize (Bootstrap hash)
        Running model ->
          Cmd.batch
            [ Id.requestIds model
            , defaultNav hash model
            ]
  in
  (appModel, initCmd)


initModel : E.Value -> E.Value -> AppModel
initModel flags ops =
  case flags |> D.decodeValue (D.null True) of
    Ok True ->
      let
        _ = Console.info "Main.initModel" "localStorage: empty"
      in
      Bootstrapping
    _ ->
      case flags |> D.decodeValue Model.decoder of
        Ok model ->
          let
            _ = Console.info "Main.initModel" ("localStorage: " ++ bytes ++ " bytes")
            bytes = model |> Console.toString |> String.length |> fromInt
          in
          Running (model |> Crdt.initDoc ops)
        Err e ->
          let
            _ = Console.logError "Main.initModel" "localStorage" e
          in
          Bootstrapping



-- VIEW


view : AppModel -> Browser.Document Msg
view appModel =
  Browser.Document
    "DM6 Elm"
    ( case appModel of
        Bootstrapping -> []
        Running model ->
          [ div
              ( Events.globalMouseHandler
                ++ appStyle
              )
              [ div
                  headerStyle
                  ( [ viewMapTitle model
                    , viewSpacer
                    ]
                    ++ Tool.viewGlobalTools model
                    ++ Search.viewSearchResult model -- TODO: move to "main" for scroll along?
                  )
              , div
                  ( [ id "main" ]
                    ++ mainStyle
                  )
                  ( [ dispatch.view model.boxId [] model ] -- boxPath = []
                    ++ Tool.viewMapTools model
                  )
              ]
          , viewMeasure model
          ]
    )


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


update : Msg -> AppModel -> (AppModel, Cmd Msg)
update msg appModel =
  let
    _ =
      case msg of
        Mouse (MouseDef.Move _) -> msg
        _ -> Console.infoGroup msg
  in
  case appModel of
    Bootstrapping -> bootstrap msg
    Running model ->
      let
        env = Env model dispatch
        outcome =
          case msg of
            -- box renderers
            TopicMap msg_ -> TMC.update msg_ env
            -- feature modules
            Tool msg_ -> Tool.update msg_ env
            Text msg_ -> Text.update msg_ env
            Mouse msg_ -> Mouse.update msg_ env
            Search msg_ -> Search.update msg_ env
            Icon msg_ -> Icon.update msg_ env
            Nav msg_ -> Nav.update msg_ env
            Id msg_ -> Id.update msg_ env
            --
            Bootstrap hash ids ->
              let
                _ = Console.logError "Main.update"
                  "Received Bootstrap message while not in Bootstrapping state" "-> ignore"
              in
              env
                |> Env.outcome
            Scrolled pos ->
              env
                |> Env.map (updateScrollPos pos)
                |> Env.outcomeDir (Directives Store NoUndo) -- ### TODO: was "Swap"
            Cancel maybeTarget ->
              env
                |> cancelUI maybeTarget
                |> Outcome.from
            NoOp ->
              env
                |> Env.outcome
      in
      outcome
        |> Outcome.mergeCmd (Id.requestIds outcome.model)
        |> Outcome.performWith model
        |> running


{- Handles the Bootstrap message.
Transitions AppModel from Bootstrapping to Running.
-}
bootstrap : Msg -> (AppModel, Cmd Msg)
bootstrap msg =
  case msg of
    Bootstrap hash ids ->
      case ids of
        id :: pool ->
          Model.bootstrap (TopicId id)
            |> Id.fillPool pool
            |> defaultContent
            |> TopicMap.defaultContent
            |> Outcome.with (defaultNav hash) (Directives Store NoUndo) -- ### TODO: was "Swap"
            |> Outcome.perform 
            |> runningWidth (domReady ())
        [] ->
          Console.logError "Main.bootstrap" "Empty ID list received in Bootstrap message"
            (Bootstrapping, Cmd.none)
    _ ->
      Console.logError "Main.bootstrap"
        "Received other than Bootstrap message while in Bootstrapping state"
        (Bootstrapping, Cmd.none)


running : (Model, Cmd Msg) -> (AppModel, Cmd Msg)
running (model, cmd) =
  (Running model, cmd)


runningWidth : Cmd Msg -> (Model, Cmd Msg) -> (AppModel, Cmd Msg)
runningWidth cmd_ (model, cmd) =
  (Running model, Cmd.batch [cmd, cmd_])


{- Sets up default content for newly created DB. This comprises a) root topic/box and b) the
underlying item set. Note: the ID for the root topic is taken from model, where it is written
while bootstrapping.
-}
defaultContent : Model -> Model
defaultContent model =
  let
    (BoxId (TopicId id as topicId)) = model.rootBoxId
    size = TextSize (Size 0 0) (Size 0 0)
    rootTopic = Topic topicId Nothing C.rootBoxName size []
    _ = Console.info "Main.defaultContent" ("root topic --> " ++ id)
  in
  model
    |> Crdt.createTopic rootTopic
    |> createRootBox topicId


createRootBox : TopicId -> Model -> Model
createRootBox topicId model =
  let
    (setId, model_) = Id.new model
    itemSet = ItemSet setId []
    boxId = BoxId topicId
    rootBox = Box boxId setId Dict.empty Renderer.default
  in
  model_
    |> Crdt.createItemSet itemSet
    |> Crdt.createBox rootBox


defaultNav : String -> Model -> Cmd Msg
defaultNav hash model =
  let
    boxId =
      case Nav.boxIdFromHash hash of
        Just boxId_ -> boxId_
        Nothing -> model.boxId
  in
  Nav.pushUrl boxId


cancelUI : Maybe Target -> Env -> (Model, Cmd Msg)
cancelUI maybeTarget env =
  env
    |> Env.map Icon.closePicker
    |> Env.map Search.closeMenu
    |> Env.map Tool.closeMenu
    |> cancelUIWith maybeTarget


cancelUIWith : Maybe Target -> Env -> (Model, Cmd Msg)
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
