port module Main exposing (..)

import Box
import Config as C
import Console
import Dict
import Dispatch exposing (dispatch)
import Env exposing (Env)
import Feature.Crdt as Crdt
import Feature.CrdtDef as CrdtDef
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


port modelReady : () -> Cmd msg

port store : (E.Value, E.Value) -> Cmd msg

port outgoing : E.Value -> Cmd msg

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
          [ Crdt.sub
          , Text.sub
          , Nav.sub
          , onScroll Scrolled
          ]
        )
    }


init : (E.Value, E.Value, String) -> (AppModel, Cmd Msg)
init (local, ops, hash) =
  let
    appModel = initModel local ops
    initCmd =
      case appModel of
        Bootstrapping ->
          Id.generateIds C.idPoolSize (Bootstrap hash)
        Running model ->
          Cmd.batch
            [ Id.requestIds model
            , initNav hash model
            , modelReady ()
            ]
  in
  (appModel, initCmd)


{- Creates initial AppModel based on passed database content.
Database content is passed separately: local state and synced state.
-}
initModel : E.Value -> E.Value -> AppModel
initModel local ops =
  case local |> D.decodeValue (D.null True) of
    Ok True ->
      let
        _ = Console.info "Main.initModel" "Bootstrap detected (no DB exists)"
      in
      Bootstrapping
    _ ->
      case Model.decode local of
        Ok localModel ->
          case CrdtDef.decodeDoc ops localModel.replicaId of
            Just (doc, docModel) ->
              let
                model = Model.init localModel (doc, docModel)
                bytes = model |> Console.toString |> String.length |> fromInt
                _ = Console.info "Main.initModel" ("Decoded model: " ++ bytes ++ " bytes")
              in
              Running model
            Nothing ->
              Bootstrapping -- decoding CRDT document failed -> creating bootstrap model
        Err e ->
          let
            _ = Console.logError "Main.initModel" "Decoding \"local\" store failed" e
          in
          Bootstrapping -- decoding "local" store failed -> creating bootstrap model



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
            Crdt msg_ -> Crdt.update msg_ env
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
                |> Env.map (setScroll pos)
                |> Env.outcomeDir (Directives Store NoUndo)
            Cancel maybeTarget ->
              env
                |> cancelUI maybeTarget
            NoOp ->
              env
                |> Env.outcome
      in
      outcome
        |> Outcome.mergeCmd (Id.requestIds outcome.model)
        |> performOutcomeWith model
        |> running


{- Handles the Bootstrap message: creates bootstrap content and transitions AppModel from
Bootstrapping to Running.
-}
bootstrap : Msg -> (AppModel, Cmd Msg)
bootstrap msg =
  case msg of
    Bootstrap hash ids ->
      case ids of
        repId :: rootId :: pool ->
          let
            replicaId = "r-" ++ repId
            rootTopicId = TopicId rootId
            _ = Console.info "Main.bootstrap" {replicaId = replicaId, rootTopicId = rootId}
          in
          Model.bootstrap replicaId rootTopicId
            |> Id.fillPool pool
            |> bootstrapContent rootTopicId
            |> TopicMap.bootstrapContent
            |> Outcome.with (initNav hash) (Directives Store NoUndo)
            |> performOutcome 
            |> runningWith (modelReady ())
        _ ->
          Console.logError "Main.bootstrap" "Not enough IDs in Bootstrap message"
            (Bootstrapping, Cmd.none)
    _ ->
      Console.logError "Main.bootstrap"
        "Received other than Bootstrap message while in Bootstrapping state"
        (Bootstrapping, Cmd.none)


{- Sets up bootstrap CRDT content for newly created DB: root topic, root box, root box ID.
-}
bootstrapContent : TopicId -> Model -> Model
bootstrapContent rootTopicId model =
  let
    size = TextSize (Size 0 0) (Size 0 0)
    rootTopic = Topic rootTopicId Nothing C.rootBoxName size []
    rootBoxId = BoxId rootTopicId
  in
  model
    |> Crdt.createTopic rootTopic -- low-level call with given ID
    |> createRootBox rootBoxId
    |> Crdt.setRootBoxId rootBoxId


createRootBox : BoxId -> Model -> Model
createRootBox boxId model =
  let
    (setId, model_) = Id.new model
    rootBox = Box boxId setId Dict.empty Renderer.default
  in
  model_
    |> Box.createItemSet (ItemSet setId [])
    |> Box.create rootBox


initNav : String -> Model -> Cmd Msg
initNav hash model =
  let
    _ = Console.info "Main.initNav" hash
  in
  case Nav.boxIdFromHash hash of
    Just boxId ->
      if boxId == model.boxId then
        Nav.setViewport model
      else
        let
          _ = Console.info "Main.initNav" ("Unexpected boxId hash: " ++ toBoxId boxId ++
            " -> using boxId from model: " ++ toBoxId model.boxId)
        in
        Nav.pushUrl model.boxId
    Nothing ->
      Nav.pushUrl model.boxId


running : (Model, Cmd Msg) -> (AppModel, Cmd Msg)
running (model, cmd) =
  (Running model, cmd)


runningWith : Cmd Msg -> (Model, Cmd Msg) -> (AppModel, Cmd Msg)
runningWith cmd_ (model, cmd) =
  (Running model, Cmd.batch [cmd, cmd_])


--

{- Perform the directives described in an outcome.
Only used for bootstrap when there is no previous model (no undo possible).
-}
performOutcome : Outcome -> (Model, Cmd Msg)
performOutcome outcome =
  performOutcomeWith outcome.model outcome


{- Perform the directives described in an outcome.
In case of an Undo directive the previous model marks the begin of an undoable action.
-}
performOutcomeWith : Model -> Outcome -> (Model, Cmd Msg)
performOutcomeWith prevModel {model, cmd, directives} =
  let
    performUndo : Model -> Model
    performUndo model_ =
      case directives.undo of
        MarkSingle -> Crdt.markUndoSingle prevModel model_
        MarkBegin -> Crdt.markUndoBegin prevModel model_
        MarkEnd -> Crdt.markUndoEnd model_
        NoUndo -> model_


    performStorage : Model -> (Model, Cmd Msg)
    performStorage model_ =
      case directives.storage of
        Store -> store_ model_ True
        StoreOnly -> store_ model_ False
        NoStore -> (model_, cmd)


    store_ : Model -> Bool -> (Model, Cmd Msg)
    store_ model_ shouldSend =
      let
        (doc, model__) = Crdt.encodeDelta model_
      in
      ( model__
      , Cmd.batch
          [ cmd
          , store (doc, Model.encode model__)
          , if shouldSend then outgoing doc else Cmd.none
          ]
      )
  in
  model
    |> performUndo
    |> performStorage


--

cancelUI : Maybe Target -> Env -> Outcome
cancelUI maybeTarget env =
  env
    |> Env.map Icon.closePicker
    |> Env.map Search.closeMenu
    |> Env.map Tool.closeMenu
    |> cancelUIWith maybeTarget


cancelUIWith : Maybe Target -> Env -> Outcome
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
    env
      |> Env.outcome -- keep selection, hover state, and edit mode
  else
    env
      |> Env.map Sel.clear
      |> Env.map Mouse.clearHover -- TODO: needed?
      |> Text.leaveEdit


setScroll : Point -> Model -> Model
setScroll pos model =
  TopicMap.setScroll model.boxId pos model
