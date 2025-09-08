port module Main exposing (..)

import AppModel exposing (..)
import Boxing exposing (boxContainer, unboxContainer)
import Config exposing (..)
import MapAutoSize exposing (autoSize)
import MapRenderer exposing (viewMap)
import Model exposing (..)
import ModelAPI exposing (..)
import Storage exposing (storeModel, storeModelWith, modelDecoder)
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



-- PORTS


port importJSON : () -> Cmd msg
port exportJSON : () -> Cmd msg



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
  ( UndoList.fresh <| initModel flags
  , Cmd.none
  )


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
  [ style "font-family" mainFont
  , style "user-select" "none"
  , style "-webkit-user-select" "none" -- Safari still needs vendor prefix
  ]


measureStyle : List (Attribute Msg)
measureStyle =
  [ style "position" "fixed"
  , style "visibility" "hidden"
  , style "white-space" "pre-wrap"
  , style "font-family" mainFont
  , style "font-size" <| fromInt contentFontSize ++ "px"
  , style "line-height" <| fromFloat topicLineHeight
  , style "padding" <| fromInt topicDetailPadding ++ "px"
  , style "width" <| fromFloat topicDetailMaxWidth ++ "px"
  , style "min-width" <| fromFloat (topicSize.w - topicSize.h) ++ "px"
  , style "max-width" "max-content"
  , style "border-width" <| fromFloat topicBorderWidth ++ "px"
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
    AddTopic -> createTopicIn topicDefaultText Nothing [ activeMap present ] present
      |> storeModel |> push undoModel
    MoveTopicToMap topicId mapId origPos targetId targetMapPath pos
      -> moveTopicToMap topicId mapId origPos targetId targetMapPath pos present
      |> storeModel |> push undoModel
    SwitchDisplay displayMode -> switchDisplay displayMode present
      |> storeModel |> swap undoModel
    Search searchMsg -> updateSearch searchMsg undoModel
    Edit editMsg -> updateEdit editMsg present |> swap undoModel
    IconMenu iconMenuMsg -> updateIconMenu iconMenuMsg present |> swap undoModel
    Mouse mouseMsg -> updateMouse mouseMsg present |> swap undoModel
    Nav navMsg -> updateNav navMsg present |> storeModel |> swap undoModel
    Hide -> hide present |> storeModel |> push undoModel
    Delete -> delete present |> storeModel |> push undoModel
    Undo -> undo undoModel
    Redo -> redo undoModel
    Import -> (present, importJSON ()) |> swap undoModel
    Export -> (present, exportJSON ()) |> swap undoModel
    NoOp -> (present, Cmd.none) |> swap undoModel


moveTopicToMap : Id -> MapId -> Point -> Id -> MapPath -> Point -> Model -> Model
moveTopicToMap topicId mapId origPos targetId targetMapPath pos model =
  let
    (newModel, created) = createMapIfNeeded targetId model
    newPos =
      case created of
        True -> Point
          (topicW2 + whiteBoxPadding)
          (topicH2 + whiteBoxPadding)
        False -> pos
    props_ =
      getTopicProps topicId mapId newModel.maps
      |> Maybe.andThen (\props -> Just (MapTopic { props | pos = newPos }))
  in
  case props_ of
    Just props ->
      newModel
      |> hideItem topicId mapId
      |> setTopicPos topicId mapId origPos
      |> addItemToMap topicId props targetId
      |> select targetId targetMapPath
      |> autoSize
    Nothing -> model


createMapIfNeeded : Id -> Model -> (Model, Bool)
createMapIfNeeded topicId model =
  if hasMap topicId model.maps then
    (model, False)
  else
    ( model
      |> createMap topicId
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
  ( case getSingleSelection model of
    Just (containerId, mapPath) ->
      let
        mapId = getMapId mapPath
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

updateEdit : EditMsg -> Model -> (Model, Cmd Msg)
updateEdit msg model =
  case msg of
    EditStart -> startEdit model
    OnTextInput text -> onTextInput text model |> storeModel
    OnTextareaInput text -> onTextareaInput text model |> storeModelWith
    SetTopicSize topicId mapId size ->
      ( model
        |> setTopicSize topicId mapId size
        |> autoSize
      , Cmd.none
      )
    EditEnd -> (endEdit model, Cmd.none)


startEdit : Model -> (Model, Cmd Msg)
startEdit model =
  let
    newModel = case getSingleSelection model of
      Just (topicId, mapPath) ->
        { model | editState = ItemEdit topicId (getMapId mapPath) }
        |> setDetailDisplayIfMonade topicId (getMapId mapPath)
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
  case getSingleSelection model of
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
        (\(itemId, mapPath) modelAcc -> hideItem itemId (getMapId mapPath) modelAcc)
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
        (\itemId modelAcc -> deleteItem itemId modelAcc)
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
  |> storeModel
  |> swap newUndoModel


redo : UndoModel -> (UndoModel, Cmd Msg)
redo undoModel =
  let
    newUndoModel = UndoList.redo undoModel
    newModel = resetTransientState newUndoModel.present
  in
  newModel
  |> storeModel
  |> swap newUndoModel
