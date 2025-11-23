module Main exposing (..)

import Box
import Box.Size as Size
import Box.Transfer as Transfer
import Config as C
import Item
import Map
import Model exposing (Model, Msg(..), NavMsg(..))
import ModelHelper exposing (..)
import Storage as S
import Tool
import Undo exposing (UndoModel)
import Utils as U
-- feature modules
import IconAPI
import MouseAPI
import SearchAPI
import SelectionAPI as Sel
import TextEditAPI

import Browser
import Html exposing (Html, Attribute, div, text, br, a)
import Html.Attributes exposing (id, style, href)
import Json.Decode as D
import Json.Encode as E
import String exposing (fromInt, fromFloat)



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
  (initModel flags, Cmd.none) |> Undo.reset


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
      appStyle
      [ Tool.viewAppHeader undoModel
      , div
        ( mainStyle
          ++ MouseAPI.hoverHandler
        )
        ( [ Map.view (Box.active present) [] present ] -- boxPath = []
          ++ SearchAPI.viewMenu present
          ++ IconAPI.viewMenu present
        )
      ]
    , viewFooter
    , viewMeasure present
    ]


appStyle : List (Attribute Msg)
appStyle =
  [ style "display" "flex"
  , style "flex-direction" "column"
  , style "height" "100%"
  , style "font-family" C.mainFont
  , style "user-select" "none"
  , style "-webkit-user-select" "none" -- Safari still needs vendor prefix
  ]


mainStyle : List (Attribute Msg)
mainStyle =
  [ style "position" "relative"
  , style "flex-grow" "1"
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
      [ text "DMX Systems" ]
    ]


footerStyle : List (Attribute Msg)
footerStyle =
  [ style "font-size" <| fromInt C.footerFontSize ++ "px"
  , style "position" "absolute"
  , style "bottom" "0"
  , style "color" "lightgray"
  ]


linkStyle : List (Attribute Msg)
linkStyle =
  [ style "color" "lightgray" ]


viewMeasure : Model -> Html Msg
viewMeasure model =
  div
    ( [ id "measure" ]
      ++ measureStyle
    )
    [ text model.edit.measureText
    , br [] []
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
    AddTopic -> addTopic present |> S.store |> Undo.push undoModel
    AddBox -> addBox present |> S.store |> Undo.push undoModel
    AddAssoc player1 player2 boxId -> addAssoc player1 player2 boxId present |> S.store
      |> Undo.push undoModel
    MoveTopicToBox topicId boxId origPos targetId targetBoxPath pos
      -> moveTopicToBox topicId boxId origPos targetId targetBoxPath pos present |> S.store
      |> Undo.push undoModel
    DraggedTopic -> present |> S.store |> Undo.swap undoModel
    ClickedItem itemId boxPath -> select itemId boxPath present |> Undo.swap undoModel
    ClickedBackground -> resetUI present |> Undo.swap undoModel
    ToggleDisplay topicId boxId -> toggleDisplay topicId boxId present |> S.store
      |> Undo.swap undoModel
    Unbox boxId targetBoxId -> unbox boxId targetBoxId present |> S.store |> Undo.swap undoModel
    Nav navMsg -> updateNav navMsg present |> S.store |> Undo.reset
    Hide -> hide present |> S.store |> Undo.push undoModel
    Delete -> delete present |> S.store |> Undo.push undoModel
    Undo -> Undo.undo undoModel
    Redo -> Undo.redo undoModel
    Import -> (present, S.importJSON ()) |> Undo.swap undoModel
    Export -> (present, S.exportJSON ()) |> Undo.swap undoModel
    NoOp -> (present, Cmd.none) |> Undo.swap undoModel
    -- feature modules
    Edit editMsg -> TextEditAPI.update editMsg undoModel
    Mouse mouseMsg -> MouseAPI.update mouseMsg undoModel
    Search searchMsg -> SearchAPI.update searchMsg undoModel
    Icon iconMenuMsg -> IconAPI.update iconMenuMsg undoModel


addTopic : Model -> Model
addTopic model =
  let
    boxId = Box.active model
  in
  case Box.byIdOrLog boxId model.boxes of
    Just box ->
      let
        (newModel, topicId) = Item.addTopic C.initTopicText Nothing model
        props = TopicV <| TopicProps
          (Point
            (C.initTopicPos.x + box.rect.x1)
            (C.initTopicPos.y + box.rect.y1)
          )
          C.topicDetailSize
          (TopicD LabelOnly)
      in
      newModel
      |> Box.addItem topicId props boxId
      |> Sel.select topicId [ boxId ]
    Nothing -> model


-- TODO: factor out addTopic() common code
addBox : Model -> Model
addBox model =
  let
    boxId = Box.active model
  in
  case Box.byIdOrLog boxId model.boxes of
    Just box ->
      let
        (newModel, topicId) = Item.addTopic C.initBoxText Nothing model
        props = TopicV <| TopicProps
          (Point
            (C.initTopicPos.x + box.rect.x1)
            (C.initTopicPos.y + box.rect.y1)
          )
          C.topicDetailSize
          (BoxD BlackBox)
      in
      newModel
      |> Box.addBox topicId
      |> Box.addItem topicId props boxId
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
    (newModel, assocId) = Item.addAssoc itemType role1 player1 role2 player2 model
    props = AssocV AssocProps
  in
  Box.addItem assocId props boxId newModel


moveTopicToBox : Id -> BoxId -> Point -> Id -> BoxPath -> Point -> Model -> Model
moveTopicToBox topicId boxId origPos targetId targetBoxPath pos model =
  let
    props_ =
      Box.topicProps topicId boxId model.boxes
      |> Maybe.andThen (\props -> Just (TopicV { props | pos = pos }))
  in
  case props_ of
    Just props ->
      model
      |> Box.hideItem topicId boxId
      |> Box.setTopicPos topicId boxId origPos
      |> Box.addItem topicId props targetId
      |> Sel.select targetId targetBoxPath
      |> Size.auto
    Nothing -> model


select : Id -> BoxPath -> Model -> (Model, Cmd Msg)
select itemId boxPath model =
  ( model
    |> Sel.select itemId boxPath
  , Cmd.none
  )


resetUI : Model -> (Model, Cmd Msg)
resetUI model =
  ( model
    |> Sel.clear
    |> IconAPI.closeMenu
    |> SearchAPI.closeMenu
  , Cmd.none
  )


toggleDisplay : Id -> BoxId -> Model -> Model
toggleDisplay topicId boxId model =
  let
    (newModel, newDisplayMode) =
      case Box.displayMode topicId boxId model.boxes of
        Just (TopicD LabelOnly) -> (model, Just <| TopicD Detail)
        Just (TopicD Detail) -> (model, Just <| TopicD LabelOnly)
        Just (BoxD BlackBox) -> (model, Just <| BoxD WhiteBox)
        Just (BoxD WhiteBox) -> (model, Just <| BoxD BlackBox)
        Just (BoxD Unboxed) ->
          ( { model | boxes = Transfer.boxContent topicId boxId model }
          , Just (BoxD BlackBox)
          )
        Nothing -> (model, Nothing)
  in
  case (newModel, newDisplayMode) of
    (newModel_, Just displayMode) ->
      newModel_
      |> Box.setDisplayMode topicId boxId displayMode
      |> Size.auto
    _ -> model


unbox : BoxId -> BoxId -> Model -> Model
unbox boxId targetBoxId model =
  { model | boxes = Transfer.unboxContent boxId targetBoxId model }
  |> Box.setDisplayMode boxId targetBoxId (BoxD Unboxed)
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
      |> Sel.clear
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
  model |> Box.updateRect boxId
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
        (\(itemId, boxPath) modelAcc -> Box.hideItem itemId (Box.firstId boxPath) modelAcc)
        model
  in
  newModel
  |> Sel.clear
  |> Size.auto


delete : Model -> Model
delete model =
  let
    newModel = model.selection.items
      |> List.map Tuple.first
      |> List.foldr
        (\itemId modelAcc -> Item.remove itemId modelAcc)
        model
  in
  newModel
  |> Sel.clear
  |> Size.auto
