module Main exposing (..)

import AutoSize as Size
import Boxing as B
import Config as C
import MapRenderer as Map
import Model exposing (Model, UndoModel, Msg(..))
import ModelAPI as A
import ModelHelper exposing (..)
import Storage as S
import Toolbar
import Utils as U
-- feature modules
import IconMenuAPI
import MouseAPI
import SearchAPI
import SelectionAPI as Sel
import TextEditAPI

import Browser
import Html exposing (Attribute, div, text, br)
import Html.Attributes exposing (id, style)
import Json.Decode as D
import Json.Encode as E
import String exposing (fromInt, fromFloat)
import UndoList



-- MAIN


main : Program E.Value UndoModel Msg
main =
  Browser.document
    { init = init
    , view = view
    , update = update
    , subscriptions = MouseAPI.subs
    }


init : E.Value -> (UndoModel, Cmd Msg)
init flags =
  (initModel flags, Cmd.none) |> A.reset


initModel : E.Value -> Model
initModel flags =
  case flags |> D.decodeValue (D.null True) of
    Ok True ->
      let
        _ = U.info "init" "localStorage: empty"
      in
      Model.init
    _ ->
      case flags |> D.decodeValue S.modelDecoder of
        Ok model ->
          let
            _ = U.info "init" ("localStorage: " ++ bytes ++ " bytes")
            bytes = model |> U.toString |> String.length |> fromInt
          in
          model
        Err e ->
          let
            _ = U.logError "init" "localStorage" e
          in
          Model.init



-- VIEW


view : UndoModel -> Browser.Document Msg
view ({present} as undoModel) =
  Browser.Document
    "DM6 Elm"
    [ div
      ( MouseAPI.hoverHandler
        ++ appStyle
      )
      ( [ Toolbar.view undoModel
        , Map.view (A.activeBox present) [] present -- boxPath = []
        ]
        ++ SearchAPI.viewMenu present
        ++ IconMenuAPI.view present
      )
    , div
      ( [ id "measure" ]
        ++ measureStyle
      )
      [ text present.edit.measureText
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
        _ -> U.info "update" msg
  in
  case msg of
    AddTopic -> addTopic present |> S.store |> A.push undoModel
    AddBox -> addBox present |> S.store |> A.push undoModel
    AddAssoc player1 player2 boxId -> addAssoc player1 player2 boxId present
      |> S.store |> A.push undoModel
    MoveTopicToBox topicId boxId origPos targetId targetBoxPath pos
      -> moveTopicToBox topicId boxId origPos targetId targetBoxPath pos present
      |> S.store |> A.push undoModel
    DraggedTopic -> present |> S.store |> A.swap undoModel
    ClickedBackground -> (resetUI present, Cmd.none) |> A.swap undoModel
    SwitchDisplay displayMode -> switchDisplay displayMode present
      |> S.store |> A.swap undoModel
    Nav navMsg -> updateNav navMsg present |> S.store |> A.reset
    Hide -> hide present |> S.store |> A.push undoModel
    Delete -> delete present |> S.store |> A.push undoModel
    Undo -> undo undoModel
    Redo -> redo undoModel
    Import -> (present, S.importJSON ()) |> A.swap undoModel
    Export -> (present, S.exportJSON ()) |> A.swap undoModel
    NoOp -> (present, Cmd.none) |> A.swap undoModel
    -- feature modules
    Edit editMsg -> TextEditAPI.update editMsg undoModel
    Mouse mouseMsg -> MouseAPI.update mouseMsg undoModel
    Search searchMsg -> SearchAPI.update searchMsg undoModel
    IconMenu iconMenuMsg -> IconMenuAPI.update iconMenuMsg undoModel


addTopic : Model -> Model
addTopic model =
  let
    boxId = A.activeBox model
  in
  case A.boxByIdOrLog boxId model.boxes of
    Just box ->
      let
        (newModel, topicId) = A.addTopic C.initTopicText Nothing model
        props = TopicV <| TopicProps
          (Point
            (C.initTopicPos.x + box.rect.x1)
            (C.initTopicPos.y + box.rect.y1)
          )
          C.topicDetailSize
          (TopicD LabelOnly)
      in
      newModel
      |> A.addItemToBox topicId props boxId
      |> Sel.select topicId [ boxId ]
    Nothing -> model


-- TODO: factor out addTopic() common code
addBox : Model -> Model
addBox model =
  let
    boxId = A.activeBox model
  in
  case A.boxByIdOrLog boxId model.boxes of
    Just box ->
      let
        (newModel, topicId) = A.addTopic C.initBoxText Nothing model
        props = TopicV <| TopicProps
          (Point
            (C.initTopicPos.x + box.rect.x1)
            (C.initTopicPos.y + box.rect.y1)
          )
          C.topicDetailSize
          (BoxD BlackBox)
      in
      newModel
      |> A.addBox topicId
      |> A.addItemToBox topicId props boxId
      |> Sel.select topicId [ boxId ]
    Nothing -> model


-- Presumption: both players exist in same box
addAssoc : Id -> Id -> BoxId -> Model -> Model
addAssoc player1 player2 boxId model =
  addAssocAndAddToBox
    "dmx.association"
    "dmx.default" player1
    "dmx.default" player2
    boxId model


-- Presumption: both players exist in same box
addAssocAndAddToBox : ItemType -> RoleType -> Id -> RoleType -> Id -> BoxId -> Model -> Model
addAssocAndAddToBox itemType role1 player1 role2 player2 boxId model =
  let
    (newModel, assocId) = A.addAssoc itemType role1 player1 role2 player2 model
    props = AssocV AssocProps
  in
  A.addItemToBox assocId props boxId newModel


moveTopicToBox : Id -> BoxId -> Point -> Id -> BoxPath -> Point -> Model -> Model
moveTopicToBox topicId boxId origPos targetId targetBoxPath pos model =
  let
    props_ =
      A.topicProps topicId boxId model.boxes
      |> Maybe.andThen (\props -> Just (TopicV { props | pos = pos }))
  in
  case props_ of
    Just props ->
      model
      |> A.hideItem topicId boxId
      |> A.setTopicPos topicId boxId origPos
      |> A.addItemToBox topicId props targetId
      |> Sel.select targetId targetBoxPath
      |> Size.auto
    Nothing -> model


switchDisplay : DisplayMode -> Model -> Model
switchDisplay displayMode model =
  ( case Sel.single model of
    Just (boxId, boxPath) ->
      let
        targetBoxId = A.firstId boxPath
      in
      { model | boxes =
        case displayMode of
          TopicD _ -> model.boxes
          BoxD BlackBox -> B.box boxId targetBoxId model
          BoxD WhiteBox -> B.box boxId targetBoxId model
          BoxD Unboxed -> B.unbox boxId targetBoxId model
      }
      |> A.setDisplayMode boxId targetBoxId displayMode
    Nothing -> model
  )
  |> Size.auto


updateNav : NavMsg -> Model -> Model
updateNav navMsg model =
  case navMsg of
    Fullscreen -> fullscreen model
    Back -> back model


fullscreen : Model -> Model
fullscreen model =
  case Sel.single model of
    Just (topicId, _) ->
      { model | boxPath = topicId :: model.boxPath }
      |> Sel.reset
      |> adjustBoxRect topicId -1
    Nothing -> model


back : Model -> Model
back model =
  let
    (boxId, boxPath, selection) =
      case model.boxPath of
        prevBoxId :: nextBoxId :: boxIds ->
          ( prevBoxId
          , nextBoxId :: boxIds
          , [(prevBoxId, nextBoxId)]
          )
        _ -> U.logError "back" "model.boxPath has a problem" (0, [0], [])
  in
  { model
  | boxPath = boxPath
  -- , selection = selection -- TODO
  }
  |> adjustBoxRect boxId 1
  |> Size.auto


-- TODO
adjustBoxRect : BoxId -> Float -> Model -> Model
adjustBoxRect boxId factor model =
  model |> A.updateBoxRect boxId
    (\rect -> Rectangle
      (rect.x1 + factor * C.nestedBoxOffset.x)
      (rect.y1 + factor * C.nestedBoxOffset.y)
      rect.x2
      rect.y2
    )


hide : Model -> Model
hide model =
  let
    newModel = model.selection.items
      |> List.foldr
        (\(itemId, boxPath) modelAcc -> A.hideItem itemId (A.firstId boxPath) modelAcc)
        model
  in
  newModel
  |> Sel.reset
  |> Size.auto


delete : Model -> Model
delete model =
  let
    newModel = model.selection.items
      |> List.map Tuple.first
      |> List.foldr
        (\itemId modelAcc -> A.removeItem itemId modelAcc)
        model
  in
  newModel
  |> Sel.reset
  |> Size.auto


-- Undo / Redo

undo : UndoModel -> (UndoModel, Cmd Msg)
undo undoModel =
  let
    newUndoModel = UndoList.undo undoModel
    newModel = Model.initTransient newUndoModel.present
  in
  newModel
  |> S.store
  |> A.swap newUndoModel


redo : UndoModel -> (UndoModel, Cmd Msg)
redo undoModel =
  let
    newUndoModel = UndoList.redo undoModel
    newModel = Model.initTransient newUndoModel.present
  in
  newModel
  |> S.store
  |> A.swap newUndoModel


--

resetUI : Model -> Model
resetUI model =
  model
  |> Sel.reset
  |> IconMenuAPI.close
  |> SearchAPI.closeMenu
