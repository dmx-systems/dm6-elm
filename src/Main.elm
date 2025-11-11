module Main exposing (..)

import AppModel exposing (..)
import Boxing exposing (box, unbox)
import Config as C
import MapAutoSize exposing (autoSize)
import MapRenderer exposing (viewMap)
import Model exposing (..)
import ModelAPI as A
import Storage exposing (store, storeWith, modelDecoder, importJSON, exportJSON)
import Toolbar exposing (viewToolbar)
import Utils exposing (..)
-- components
import IconMenuAPI exposing (viewIconMenu, updateIconMenu)
import MouseAPI exposing (mouseHoverHandler, mouseSubs, updateMouse)
import SearchAPI exposing (viewResultMenu, updateSearch)

import Browser
import Browser.Dom as Dom
import Dict
import Html exposing (Attribute, div, text, br)
import Html.Attributes exposing (id, style)
import Json.Decode as D
import Json.Encode as E
import String exposing (fromInt, fromFloat)
import Task
import UndoList



-- MAIN


main : Program E.Value UndoModel Msg
main =
  Browser.document
    { init = init
    , view = view
    , update = update
    , subscriptions = mouseSubs
    }


init : E.Value -> (UndoModel, Cmd Msg)
init flags =
  (initModel flags, Cmd.none) |> A.reset


initModel : E.Value -> Model
initModel flags =
  case flags |> D.decodeValue (D.null True) of
    Ok True ->
      let
        _ = info "init" "localStorage: empty"
      in
      default
    _ ->
      case flags |> D.decodeValue modelDecoder of
        Ok model ->
          let
            _ = info "init"
              ("localStorage: " ++ (model |> toString |> String.length |> fromInt) ++ " bytes")
          in
          model
        Err e ->
          let
            _ = logError "init" "localStorage" e
          in
          default



-- VIEW


view : UndoModel -> Browser.Document Msg
view ({present} as undoModel) =
  Browser.Document
    "DM6 Elm"
    [ div
      ( mouseHoverHandler
        ++ appStyle
      )
      ( [ viewToolbar undoModel
        , viewMap (A.activeMap present) [] present -- boxPath = []
        ]
        ++ viewResultMenu present
        ++ viewIconMenu present
      )
    , div
      ( [ id "measure" ]
        ++ measureStyle
      )
      [ text present.measureText
      , br [] []
      ]
    ]


appStyle : List (Attribute Msg)
appStyle =
  [ style "font-family" C.mainFont
  , style "user-select" "none"
  , style "-webkit-user-select" "none" -- Safari still needs vendor prefix
  ]


measureStyle : List (Attribute Msg)
measureStyle =
  [ style "position" "fixed"
  , style "visibility" "hidden"
  , style "white-space" "pre-wrap"
  , style "font-family" C.mainFont
  , style "font-size" <| fromInt C.contentFontSize ++ "px"
  , style "line-height" <| fromFloat C.topicLineHeight
  , style "padding" <| fromInt C.topicDetailPadding ++ "px"
  , style "width" <| fromFloat C.topicDetailMaxWidth ++ "px"
  , style "min-width" <| fromFloat (C.topicSize.w - C.topicSize.h) ++ "px"
  , style "max-width" "max-content"
  , style "border-width" <| fromFloat C.topicBorderWidth ++ "px"
  , style "border-style" "solid"
  , style "box-sizing" "border-box"
  ]



-- UPDATE


update : Msg -> UndoModel -> (UndoModel, Cmd Msg)
update msg ({present} as undoModel) =
  let
    _ =
      case msg of
        Mouse _ -> msg
        _ -> info "update" msg
  in
  case msg of
    AddTopic -> addTopic present |> store |> A.push undoModel
    AddBox -> addBox present |> store |> A.push undoModel
    MoveTopicToMap topicId mapId origPos targetId targetMapPath pos
      -> moveTopicToMap topicId mapId origPos targetId targetMapPath pos present
      |> store |> A.push undoModel
    SwitchDisplay displayMode -> switchDisplay displayMode present
      |> store |> A.swap undoModel
    Search searchMsg -> updateSearch searchMsg undoModel
    Edit editMsg -> updateEdit editMsg undoModel
    IconMenu iconMenuMsg -> updateIconMenu iconMenuMsg undoModel
    Mouse mouseMsg -> updateMouse mouseMsg undoModel
    Nav navMsg -> updateNav navMsg present |> store |> A.reset
    Hide -> hide present |> store |> A.push undoModel
    Delete -> delete present |> store |> A.push undoModel
    Undo -> undo undoModel
    Redo -> redo undoModel
    Import -> (present, importJSON ()) |> A.swap undoModel
    Export -> (present, exportJSON ()) |> A.swap undoModel
    NoOp -> (present, Cmd.none) |> A.swap undoModel


addTopic : Model -> Model
addTopic model =
  let
    mapId = A.activeMap model
  in
  case A.mapByIdOrLog mapId model.boxes of
    Just map ->
      let
        (newModel, topicId) = A.addTopic C.initTopicText Nothing model
        props = MapTopic <| TopicProps
          (Point
            (C.initTopicPos.x + map.rect.x1)
            (C.initTopicPos.y + map.rect.y1)
          )
          C.topicDetailSize
          (Monad LabelOnly)
      in
      newModel
      |> A.putItemOnMap topicId props mapId
      |> A.select topicId [ mapId ]
    Nothing -> model


-- TODO: factor out addTopic() common code
addBox : Model -> Model
addBox model =
  let
    mapId = A.activeMap model
  in
  case A.mapByIdOrLog mapId model.boxes of
    Just map ->
      let
        (newModel, topicId) = A.addTopic C.initBoxText Nothing model
        props = MapTopic <| TopicProps
          (Point
            (C.initTopicPos.x + map.rect.x1)
            (C.initTopicPos.y + map.rect.y1)
          )
          C.topicDetailSize
          (Box BlackBox)
      in
      newModel
      |> A.addMap topicId
      |> A.putItemOnMap topicId props mapId
      |> A.select topicId [ mapId ]
    Nothing -> model


moveTopicToMap : Id -> BoxId -> Point -> Id -> BoxPath -> Point -> Model -> Model
moveTopicToMap topicId mapId origPos targetId targetMapPath pos model =
  let
    props_ =
      A.topicProps topicId mapId model.boxes
      |> Maybe.andThen (\props -> Just (MapTopic { props | pos = pos }))
  in
  case props_ of
    Just props ->
      model
      |> A.hideItem topicId mapId
      |> A.setTopicPos topicId mapId origPos
      |> A.putItemOnMap topicId props targetId
      |> A.select targetId targetMapPath
      |> autoSize
    Nothing -> model


switchDisplay : DisplayMode -> Model -> Model
switchDisplay displayMode model =
  ( case A.singleSelection model of
    Just (boxId, boxPath) ->
      let
        mapId = A.firstId boxPath
      in
      { model | boxes =
        case displayMode of
          Monad _ -> model.boxes
          Box BlackBox -> box boxId mapId model
          Box WhiteBox -> box boxId mapId model
          Box Unboxed -> unbox boxId mapId model
      }
      |> A.setDisplayMode boxId mapId displayMode
    Nothing -> model
  )
  |> autoSize


-- Text Edit

updateEdit : EditMsg -> UndoModel -> (UndoModel, Cmd Msg)
updateEdit msg ({present} as undoModel) =
  case msg of
    EditStart -> startEdit present |> A.push undoModel
    OnTextInput text -> onTextInput text present |> store |> A.swap undoModel
    OnTextareaInput text -> onTextareaInput text present |> storeWith |> A.swap undoModel
    SetTopicSize topicId mapId size ->
      ( present
        |> A.setTopicSize topicId mapId size
        |> autoSize
      , Cmd.none
      )
      |> A.swap undoModel
    EditEnd ->
      (endEdit present, Cmd.none)
      |> A.swap undoModel


startEdit : Model -> (Model, Cmd Msg)
startEdit model =
  let
    newModel = case A.singleSelection model of
      Just (topicId, boxPath) ->
        { model | editState = ItemEdit topicId (A.firstId boxPath) }
        |> setDetailDisplayIfMonade topicId (A.firstId boxPath)
        |> autoSize
      Nothing -> model
  in
  (newModel, focus newModel)


setDetailDisplayIfMonade : Id -> BoxId -> Model -> Model
setDetailDisplayIfMonade topicId mapId model =
  model |> A.updateTopicProps topicId mapId
    (\props ->
      case props.displayMode of
        Monad _ -> { props | displayMode = Monad Detail }
        _ -> props
    )


onTextInput : String -> Model -> Model
onTextInput text model =
  case model.editState of
    ItemEdit topicId _ ->
      A.updateTopicInfo topicId
        (\topic -> { topic | text = text })
        model
    NoEdit -> logError "onTextInput" "called when editState is NoEdit" model


onTextareaInput : String -> Model -> (Model, Cmd Msg)
onTextareaInput text model =
  case model.editState of
    ItemEdit topicId mapId ->
      A.updateTopicInfo topicId
        (\topic -> { topic | text = text })
        model
      |> measureText text topicId mapId
    NoEdit -> logError "onTextareaInput" "called when editState is NoEdit" (model, Cmd.none)


measureText : String -> Id -> BoxId -> Model -> (Model, Cmd Msg)
measureText text topicId mapId model =
  ( { model | measureText = text }
  , Dom.getElement "measure"
    |> Task.attempt
      (\result ->
        case result of
          Ok elem -> Edit
            (SetTopicSize topicId mapId
              (Size elem.element.width elem.element.height)
            )
          Err err -> logError "measureText" (toString err) NoOp
      )
  )


endEdit : Model -> Model
endEdit model =
  { model | editState = NoEdit }
  |> autoSize


focus : Model -> Cmd Msg
focus model =
  let
    nodeId =
      case model.editState of
        ItemEdit id mapId -> "dmx-input-" ++ fromInt id ++ "-" ++ fromInt mapId
        NoEdit -> logError "focus" "called when editState is NoEdit" ""
  in
  Dom.focus nodeId |> Task.attempt
    (\result ->
      case result of
        Ok () -> NoOp
        Err e -> logError "focus" (toString e) NoOp
    )


--

updateNav : NavMsg -> Model -> Model
updateNav navMsg model =
  case navMsg of
    Fullscreen -> fullscreen model
    Back -> back model


fullscreen : Model -> Model
fullscreen model =
  case A.singleSelection model of
    Just (topicId, _) ->
      { model | boxPath = topicId :: model.boxPath }
      |> A.resetSelection
      |> adjustMapRect topicId -1
    Nothing -> model


back : Model -> Model
back model =
  let
    (mapId, boxPath, selection) =
      case model.boxPath of
        prevMapId :: nextMapId :: mapIds ->
          ( prevMapId
          , nextMapId :: mapIds
          , [(prevMapId, nextMapId)]
          )
        _ -> logError "back" "model.boxPath has a problem" (0, [0], [])
  in
  { model
  | boxPath = boxPath
  -- , selection = selection -- TODO
  }
  |> adjustMapRect mapId 1
  |> autoSize


adjustMapRect : BoxId -> Float -> Model -> Model
adjustMapRect mapId factor model =
  model |> A.updateMapRect mapId
    (\rect -> Rectangle
      (rect.x1 + factor * 400) -- TODO
      (rect.y1 + factor * 300) -- TODO
      rect.x2
      rect.y2
    )


hide : Model -> Model
hide model =
  let
    newModel = model.selection
      |> List.foldr
        (\(itemId, boxPath) modelAcc -> A.hideItem itemId (A.firstId boxPath) modelAcc)
        model
  in
  newModel
  |> A.resetSelection
  |> autoSize


delete : Model -> Model
delete model =
  let
    newModel = model.selection
      |> List.map Tuple.first
      |> List.foldr
        (\itemId modelAcc -> A.removeItem itemId modelAcc)
        model
  in
  newModel
  |> A.resetSelection
  |> autoSize


-- Undo / Redo

undo : UndoModel -> (UndoModel, Cmd Msg)
undo undoModel =
  let
    newUndoModel = UndoList.undo undoModel
    newModel = resetTransientState newUndoModel.present
  in
  newModel
  |> store
  |> A.swap newUndoModel


redo : UndoModel -> (UndoModel, Cmd Msg)
redo undoModel =
  let
    newUndoModel = UndoList.redo undoModel
    newModel = resetTransientState newUndoModel.present
  in
  newModel
  |> store
  |> A.swap newUndoModel
