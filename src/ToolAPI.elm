module ToolAPI exposing (viewAppHeader, viewTools, update)

import Box
import Box.Size as Size
import Box.Transfer as Transfer
import Config as C
import Item
import Model exposing (Model, Msg(..))
import ModelHelper exposing (..)
import Storage as S
import Undo exposing (UndoModel)
import Utils as U
-- feature modules
import Icon
import IconAPI
import Nav
import Search
import SearchAPI
import SelectionAPI as Sel
import TextEdit as T
import Tool

import Html exposing (Html, Attribute, div, span, text, button)
import Html.Attributes exposing (style, title, disabled)
import Html.Events exposing (onClick)
import String exposing (fromInt)



-- VIEW


-- Global Tools

viewAppHeader : UndoModel -> Html Msg
viewAppHeader ({present} as undoModel) =
  div
    appHeaderStyle
    [ viewMapTitle present
    , div spacerStyle []
    , div
      []
      [ viewButton "Add Topic" (Tool Tool.AddTopic) always undoModel
      , viewButton "Add Box" (Tool Tool.AddBox) always undoModel
      ]
    , SearchAPI.viewInput present
    , div
      []
      [ viewButton "Undo" (Tool Tool.Undo) Undo.hasPast undoModel
      , viewButton "Redo" (Tool Tool.Redo) Undo.hasFuture undoModel
      ]
    , div
      []
      [ viewButton "Import" (Tool Tool.Import) always undoModel
      , viewButton "Export" (Tool Tool.Export) always undoModel
      ]
    ]


appHeaderStyle : List (Attribute Msg)
appHeaderStyle =
  [ style "display" "flex"
  , style "align-items" "center"
  , style "gap" "18px"
  , style "height" <| fromInt C.appHeaderHeight ++ "px"
  , style "padding" "0 8px"
  , style "background-color" C.toolbarColor
  ]


viewMapTitle : Model -> Html Msg
viewMapTitle model =
  div
    mapTitleStyle
    [ text <| Box.activeName model ]


mapTitleStyle : List (Attribute Msg)
mapTitleStyle =
  [ style "font-size" "24px"
  , style "font-weight" "bold"
  ]


spacerStyle : List (Attribute Msg)
spacerStyle =
  [ style "flex-grow" "1" ]


viewButton : String -> Msg -> (UndoModel -> Bool) -> UndoModel -> Html Msg
viewButton label msg isEnabled undoModel =
  button
    ( [ onClick msg
      , disabled <| not <| isEnabled undoModel
      , U.stopPropagationOnMousedown NoOp
      ]
      ++ buttonStyle
    )
    [ text label ]


buttonStyle : List (Attribute Msg)
buttonStyle =
  [ style "font-family" C.mainFont
  , style "font-size" <| fromInt C.toolFontSize ++ "px"
  ]


{-| isEnabled predicate -}
always : UndoModel -> Bool
always undoModel =
  True


-- Item Tools

viewTools : Id -> BoxId -> Model -> List (Html Msg)
viewTools itemId boxId model =
  [ viewToolbar itemId boxId model
  , viewCaret itemId boxId model
  ]


viewToolbar : Id -> BoxId -> Model -> Html Msg
viewToolbar itemId boxId model =
  let
    topicTools =
      [ viewIconButton "Edit" "edit-3" False (Edit T.EditStart)
      , viewIconButton "Set Icon" "image" False (Icon Icon.OpenMenu)
      , viewIconButton "Traverse" "share-2" False (Search Search.ShowRelated)
      , viewIconButton "Delete" "trash" False (Tool Tool.Delete)
      , viewIconButton "Remove" "x" False (Tool Tool.Hide) -- TODO: "hide" -> "remove"
      ]
    boxTools =
      if Item.isBox itemId model then
        [ viewSpacer
        , viewIconButton "Unbox" "external-link" isUnboxed (Tool <| Tool.Unbox itemId boxId)
        , viewIconButton "Fullscreen" "maximize-2" False (Nav Nav.Fullscreen)
        ]
      else
        []
    isUnboxed =
      Box.displayMode itemId boxId model == Just (BoxD Unboxed)
  in
  div
    (toolbarStyle itemId boxId model)
    (topicTools ++ boxTools)


toolbarStyle : Id -> BoxId -> Model -> List (Attribute Msg)
toolbarStyle topicId boxId model =
  [ style "position" "absolute"
  , style "top" "-32px"
  , style "white-space" "nowrap"
  , style "background-color" C.toolbarColor
  ]


viewSpacer : Html Msg
viewSpacer =
  span
    [ style "display" "inline-block"
    , style "width" "12px"
    ]
    []


viewCaret : Id -> BoxId -> Model -> Html Msg
viewCaret itemId boxId model =
  let
    icon =
      case Box.displayMode itemId boxId model of
        Just (TopicD LabelOnly) -> "chevron-right"
        Just (TopicD Detail) -> "chevron-down"
        Just (BoxD BlackBox) -> "chevron-right"
        Just (BoxD WhiteBox) -> "chevron-down"
        Just (BoxD Unboxed) -> "chevron-down"
        Nothing -> "??"
  in
  button
    ( [ onClick <| Tool <| Tool.ToggleDisplay itemId boxId
      , U.stopPropagationOnMousedown NoOp
      ]
      ++ caretStyle
    )
    [ IconAPI.viewIcon icon 20 ]


caretStyle : List (Attribute Msg)
caretStyle  =
  [ style "position" "absolute"
  , style "top" "-1px"
  , style "left" "-35px"
  ]


viewIconButton : String -> String -> Bool -> Msg -> Html Msg
viewIconButton label icon disabled_ msg =
  button
    [ onClick msg
    , disabled disabled_
    , title label
    , U.stopPropagationOnMousedown NoOp
    ]
    [ IconAPI.viewIcon icon 20 ]



-- UPDATE


update : Tool.Msg -> UndoModel -> (UndoModel, Cmd Msg)
update msg ({present} as undoModel) =
  case msg of
    -- Global Tools
    Tool.AddTopic -> addTopic present |> S.store |> Undo.push undoModel
    Tool.AddBox -> addBox present |> S.store |> Undo.push undoModel
    Tool.Undo -> Undo.undo undoModel
    Tool.Redo -> Undo.redo undoModel
    Tool.Import -> (present, S.importJSON ()) |> Undo.swap undoModel
    Tool.Export -> (present, S.exportJSON ()) |> Undo.swap undoModel
    -- Item Tools
    Tool.ToggleDisplay topicId boxId -> toggleDisplay topicId boxId present |> S.store
      |> Undo.swap undoModel
    Tool.Unbox boxId targetBoxId -> unbox boxId targetBoxId present |> S.store
      |> Undo.swap undoModel
    Tool.Hide -> hide present |> S.store |> Undo.push undoModel
    Tool.Delete -> delete present |> S.store |> Undo.push undoModel


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


toggleDisplay : Id -> BoxId -> Model -> Model
toggleDisplay topicId boxId model =
  let
    (newModel, newDisplayMode) =
      case Box.displayMode topicId boxId model of
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
