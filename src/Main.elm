module Main exposing (..)

import AppModel exposing (..)
import Boxing exposing (boxContainer, unboxContainer)
import Config as C
import MapAutoSize exposing (autoSize)
import MapRenderer exposing (viewMap)
import Model exposing (..)
import ModelAPI exposing (..)
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
  (initModel flags, Cmd.none) |> reset


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
        , viewMap (activeMap present) [] present -- mapPath = []
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
    AddTopic -> addTopicAndPutOnMap C.topicDefaultText Nothing [ activeMap present ] present
      |> store |> push undoModel
    MoveTopicToMap topicId mapId origPos targetId targetMapPath pos
      -> moveTopicToMap topicId mapId origPos targetId targetMapPath pos present
      |> store |> push undoModel
    SwitchDisplay displayMode -> switchDisplay displayMode present
      |> store |> swap undoModel
    Search searchMsg -> updateSearch searchMsg undoModel
    Edit editMsg -> updateEdit editMsg undoModel
    IconMenu iconMenuMsg -> updateIconMenu iconMenuMsg undoModel
    Mouse mouseMsg -> updateMouse mouseMsg undoModel
    Nav navMsg -> updateNav navMsg present |> store |> reset
    Hide -> hide present |> store |> push undoModel
    Delete -> delete present |> store |> push undoModel
    Undo -> undo undoModel
    Redo -> redo undoModel
    Import -> (present, importJSON ()) |> swap undoModel
    Export -> (present, exportJSON ()) |> swap undoModel
    NoOp -> (present, Cmd.none) |> swap undoModel


addTopicAndPutOnMap : String -> Maybe IconName -> MapPath -> Model -> Model
addTopicAndPutOnMap text iconName mapPath model =
  let
    mapId = firstId mapPath
  in
  case mapByIdOrLog mapId model.maps of
    Just map ->
      let
        (newModel, topicId) = addTopic text iconName model
        props = MapTopic <| TopicProps
          (Point
            (C.newTopicPos.x + map.rect.x1)
            (C.newTopicPos.y + map.rect.y1)
          )
          C.topicDetailSize
          (Monad LabelOnly)
      in
      newModel
      |> putItemOnMap topicId props mapId
      |> select topicId mapPath
    Nothing -> model


moveTopicToMap : Id -> MapId -> Point -> Id -> MapPath -> Point -> Model -> Model
moveTopicToMap topicId mapId origPos targetId targetMapPath pos model =
  let
    (newModel, created) = createMapIfNeeded targetId model
    newPos =
      case created of
        True -> Point
          (C.topicW2 + C.whiteBoxPadding)
          (C.topicH2 + C.whiteBoxPadding)
        False -> pos
    props_ =
      topicProps topicId mapId newModel.maps
      |> Maybe.andThen (\props -> Just (MapTopic { props | pos = newPos }))
  in
  case props_ of
    Just props ->
      newModel
      |> hideItem topicId mapId
      |> setTopicPos topicId mapId origPos
      |> putItemOnMap topicId props targetId
      |> select targetId targetMapPath
      |> autoSize
    Nothing -> model


createMapIfNeeded : Id -> Model -> (Model, Bool)
createMapIfNeeded topicId model =
  if hasMap topicId model.maps then
    (model, False)
  else
    ( model
      |> addMap topicId
      |> setDisplayModeInAllMaps topicId (Container BlackBox)
      -- A nested topic which becomes a container might exist in other maps as well, still as
      -- a monad. We must set the topic's display mode to "container" in *all* maps. Otherwise
      -- in the other maps it might be revealed still as a monad.
    , True
    )


setDisplayModeInAllMaps : Id -> DisplayMode -> Model -> Model
setDisplayModeInAllMaps topicId displayMode model =
  model.maps |> Dict.foldr
    (\mapId _ modelAcc ->
      case isItemInMap topicId mapId model of
        True -> setDisplayMode topicId mapId displayMode modelAcc
        False -> modelAcc
    )
    model


switchDisplay : DisplayMode -> Model -> Model
switchDisplay displayMode model =
  ( case singleSelection model of
    Just (containerId, mapPath) ->
      let
        mapId = firstId mapPath
      in
      { model | maps =
        case displayMode of
          Monad _ -> model.maps
          Container BlackBox -> boxContainer containerId mapId model
          Container WhiteBox -> boxContainer containerId mapId model
          Container Unboxed -> unboxContainer containerId mapId model
      }
      |> setDisplayMode containerId mapId displayMode
    Nothing -> model
  )
  |> autoSize


-- Text Edit

updateEdit : EditMsg -> UndoModel -> (UndoModel, Cmd Msg)
updateEdit msg ({present} as undoModel) =
  case msg of
    EditStart -> startEdit present |> push undoModel
    OnTextInput text -> onTextInput text present |> store |> swap undoModel
    OnTextareaInput text -> onTextareaInput text present |> storeWith |> swap undoModel
    SetTopicSize topicId mapId size ->
      ( present
        |> setTopicSize topicId mapId size
        |> autoSize
      , Cmd.none
      )
      |> swap undoModel
    EditEnd ->
      (endEdit present, Cmd.none)
      |> swap undoModel


startEdit : Model -> (Model, Cmd Msg)
startEdit model =
  let
    newModel = case singleSelection model of
      Just (topicId, mapPath) ->
        { model | editState = ItemEdit topicId (firstId mapPath) }
        |> setDetailDisplayIfMonade topicId (firstId mapPath)
        |> autoSize
      Nothing -> model
  in
  (newModel, focus newModel)


setDetailDisplayIfMonade : Id -> MapId -> Model -> Model
setDetailDisplayIfMonade topicId mapId model =
  model |> updateTopicProps topicId mapId
    (\props ->
      case props.displayMode of
        Monad _ -> { props | displayMode = Monad Detail }
        _ -> props
    )


onTextInput : String -> Model -> Model
onTextInput text model =
  case model.editState of
    ItemEdit topicId _ ->
      updateTopicInfo topicId
        (\topic -> { topic | text = text })
        model
    NoEdit -> logError "onTextInput" "called when editState is NoEdit" model


onTextareaInput : String -> Model -> (Model, Cmd Msg)
onTextareaInput text model =
  case model.editState of
    ItemEdit topicId mapId ->
      updateTopicInfo topicId
        (\topic -> { topic | text = text })
        model
      |> measureText text topicId mapId
    NoEdit -> logError "onTextareaInput" "called when editState is NoEdit" (model, Cmd.none)


measureText : String -> Id -> MapId -> Model -> (Model, Cmd Msg)
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
  case singleSelection model of
    Just (topicId, _) ->
      { model | mapPath = topicId :: model.mapPath }
      |> resetSelection
      |> createMapIfNeeded topicId
      |> Tuple.first
      |> adjustMapRect topicId -1
    Nothing -> model


back : Model -> Model
back model =
  let
    (mapId, mapPath, selection) =
      case model.mapPath of
        prevMapId :: nextMapId :: mapIds ->
          ( prevMapId
          , nextMapId :: mapIds
          , [(prevMapId, nextMapId)]
          )
        _ -> logError "back" "model.mapPath has a problem" (0, [0], [])
  in
  { model
  | mapPath = mapPath
  -- , selection = selection -- TODO
  }
  |> adjustMapRect mapId 1
  |> autoSize


adjustMapRect : MapId -> Float -> Model -> Model
adjustMapRect mapId factor model =
  model |> updateMapRect mapId
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
        (\(itemId, mapPath) modelAcc -> hideItem itemId (firstId mapPath) modelAcc)
        model
  in
  newModel
  |> resetSelection
  |> autoSize


delete : Model -> Model
delete model =
  let
    newModel = model.selection
      |> List.map Tuple.first
      |> List.foldr
        (\itemId modelAcc -> removeItem itemId modelAcc)
        model
  in
  newModel
  |> resetSelection
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
  |> swap newUndoModel


redo : UndoModel -> (UndoModel, Cmd Msg)
redo undoModel =
  let
    newUndoModel = UndoList.redo undoModel
    newModel = resetTransientState newUndoModel.present
  in
  newModel
  |> store
  |> swap newUndoModel
