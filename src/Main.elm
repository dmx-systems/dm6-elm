port module Main exposing (..)

import Assoc
import Box
import Config as C
import Env exposing (Env)
import ExtManager
import Feature.Icon as Icon
import Feature.Nav as Nav
import Feature.Search as Search
import Feature.Sel as Sel
import Feature.Text as Text
import Feature.Tool as Tool
import Model exposing (Model, Msg(..))
import ModelBase exposing (..)
import Storage as S
import TopicMap.Mouse as Mouse
import TopicMap.Controller as TMC
import TopicMap.TopicMap as TM
import TopicMap.TopicMapDef as TopicMapDef
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
        ( Mouse.dragHandler
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
            ( [ ExtManager.ext.view present.boxId [] present ] -- boxPath = []
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
      , ext = ExtManager.ext
      }
    _ =
      case msg of
        TopicMap (TopicMapDef.Move _) -> msg
        _ -> U.info "Main.update" msg
  in
  case msg of
    -- gestures detected by Mouse module
    CreateAssoc topicId1 topicId2 boxId -> createAssoc topicId1 topicId2 boxId present
      |> S.store |> Undo.push undoModel
    TopicDropped topicId boxId origPos targetId targetPath -> moveTopicToBox topicId boxId
      origPos targetId targetPath present |> S.storeWith |> Undo.push undoModel
    TopicDragged -> present |> S.store |> Undo.swap undoModel
    ItemClicked itemId boxPath -> select itemId boxPath present |> Undo.swap undoModel
    Cancel maybeTarget -> cancelUI maybeTarget env |> Undo.swap undoModel
    -- renderer modules
    TopicMap topicMapMsg -> TMC.update topicMapMsg env
    -- feature modules
    Tool toolMsg -> Tool.update toolMsg env
    Text textMsg -> Text.update textMsg env
    Search searchMsg -> Search.update searchMsg env
    Icon iconMenuMsg -> Icon.update iconMenuMsg undoModel
    Nav navMsg -> Nav.update navMsg env
    --
    Scrolled pos -> updateScrollPos pos present |> S.store |> Undo.swap undoModel
    NoOp -> (undoModel, Cmd.none)


-- Presumption: both topics exist in same box
createAssoc : TopicId -> TopicId -> BoxId -> Model -> Model
createAssoc topicId1 topicId2 boxId model =
  createAssocAndAddToBox Association topicId1 topicId2 boxId model


-- Presumption: both topics exist in same box
createAssocAndAddToBox : AssocType -> TopicId -> TopicId -> BoxId -> Model -> Model
createAssocAndAddToBox assocType topicId1 topicId2 boxId model =
  let
    (newModel, assocId) = Assoc.create assocType topicId1 topicId2 model
  in
  newModel
    |> Box.addAssoc assocId boxId


moveTopicToBox : TopicId -> BoxId -> Point -> TopicId -> BoxPath -> Model -> (Model, Cmd Msg)
moveTopicToBox topicId boxId origPos targetTopicId targetPath model =
  let
    targetBoxId = BoxId targetTopicId -- after createBoxOnDemand target topic is a box for sure
    expansion = Box.expansionOf topicId boxId model
  in
  model
    |> Tool.createBoxOnDemand targetTopicId
    |> Box.addTopic (BoxTopic topicId expansion) targetBoxId
    |> Box.removeTopic topicId boxId
    |> Sel.select (T targetTopicId) targetPath
    |> TM.setTopicPos topicId boxId origPos -- TODO: dispatch via ExtManager
    |> TM.addTopic topicId targetBoxId Random -- TODO: dispatch via ExtManager


select : ItemId -> BoxPath -> Model -> (Model, Cmd Msg)
select itemId boxPath model =
  ( model
      |> Sel.select itemId boxPath
  , Cmd.none
  )


cancelUI : Maybe Target -> Env -> (Model, Cmd Msg)
cancelUI maybeTarget ({model} as env) =
  model
    |> Icon.closePicker
    |> Search.closeMenu
    |> Tool.closeMenu
    |> Env.withModel env
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
    ( model, Cmd.none ) -- keep selection, hover state, and edit mode
  else
    model
      |> Sel.clear
      |> Mouse.clearHover
      |> Env.withModel env
      |> Text.leaveEdit


updateScrollPos : Point -> Model -> Model
updateScrollPos pos model =
  TM.updateScrollPos model.boxId (\_ -> pos) model
