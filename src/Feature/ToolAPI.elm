module Feature.ToolAPI exposing (viewGlobalTools, viewMapTools, viewItemTools, update)

import Box
import Box.Size as Size
import Box.Transfer as Transfer
import Config as C
import Feature.Icon as Icon
import Feature.IconAPI as IconAPI
import Feature.MouseAPI as MouseAPI
import Feature.Nav as Nav
import Feature.Search as Search
import Feature.SearchAPI as SearchAPI
import Feature.SelAPI as SelAPI
import Feature.TextEdit as T
import Feature.Tool as Tool
import Item
import Model exposing (Model, Msg(..))
import ModelParts exposing (..)
import Storage as S
import Undo exposing (UndoModel)
import Utils as U

import Html exposing (Html, Attribute, div, span, text, button)
import Html.Attributes exposing (class, style, title, disabled)
import Html.Events exposing (onClick)
import String exposing (fromInt)



-- VIEW


-- Global Tools

viewGlobalTools : UndoModel -> List (Html Msg)
viewGlobalTools {present} =
  [ SearchAPI.viewInput present
  , div
    []
    [ viewTextButton "Import" (Tool Tool.Import) True
    , viewTextButton "Export" (Tool Tool.Export) True
    ]
  ]


-- Map Tools

viewMapTools : UndoModel -> List (Html Msg)
viewMapTools undoModel =
  [ div
    mapToolsStyle
    [ viewMapButton "Add Topic" "plus-circle" (Tool Tool.AddTopic) True
    , viewMapButton "Add Box" "plus-square" (Tool Tool.AddBox) True
    , viewSpacer
    , viewMapButton "Undo" "rotate-ccw" (Tool Tool.Undo) (Undo.hasPast undoModel)
    , viewMapButton "Redo" "rotate-cw" (Tool Tool.Redo) (Undo.hasFuture undoModel)
    ]
  ]


mapToolsStyle : List (Attribute Msg)
mapToolsStyle =
  [ style "position" "fixed"
  , style "bottom" "20px"
  , style "left" "4px"
  ]


-- Item Tools

viewItemTools : Id -> BoxId -> Model -> List (Html Msg)
viewItemTools itemId boxId model =
  let
    toolbar =
      case SelAPI.isSelected itemId boxId model of
        True -> [ viewItemToolbar itemId boxId model ]
        False -> []
    caret =
      case MouseAPI.isHovered itemId boxId model of
        True -> [ viewCaret itemId boxId model ]
        False ->[]
  in
  toolbar ++ caret


viewItemToolbar : Id -> BoxId -> Model -> Html Msg
viewItemToolbar itemId boxId model =
  let
    topicTools =
      [ viewItemButton "Edit" "edit-3" (Edit T.EditStart) True
      , viewItemButton "Set Icon" "image" (Icon Icon.OpenMenu) True
      , viewItemButton "Traverse" "share-2" (Search Search.ShowRelated) True
      , viewItemButton "Delete" "trash" (Tool Tool.Delete) True
      , viewItemButton "Remove" "x" (Tool Tool.Hide) True -- TODO: "hide" -> "remove"
      ]
    boxTools =
      if Item.isBox itemId model then
        [ viewSpacer
        , viewItemButton "Unbox" "external-link" (Tool <| Tool.Unbox itemId boxId) (not unboxed)
        , viewItemButton "Fullscreen" "maximize-2" (Nav Nav.Fullscreen) True
        ]
      else
        []
    unboxed =
      Box.displayMode itemId boxId model == Just (BoxD Unboxed)
  in
  div
    ( itemToolbarStyle itemId boxId model )
    ( topicTools
      ++ boxTools
      ++ IconAPI.viewMenu model
      ++ SearchAPI.viewTraversalResult model
    )


itemToolbarStyle : Id -> BoxId -> Model -> List (Attribute Msg)
itemToolbarStyle itemId boxId model =
  let
    offset =
      case Box.displayMode itemId boxId model of
        Just (TopicD Detail) -> 1
        Just (BoxD BlackBox) -> 1
        _ -> 0
  in
  [ style "position" "absolute"
  , style "top" <| fromInt (offset - 30) ++ "px"
  , style "left" <| fromInt (offset - 1) ++ "px"
  , style "white-space" "nowrap"
  , style "background-color" C.toolbarColor
  , style "border-radius" <| fromInt C.topicRadius ++ "px"
  , style "padding" "4px 3px 0"
  ]


viewSpacer : Html Msg
viewSpacer =
  span
    [ style "display" "inline-block"
    , style "width" "14px"
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
caretStyle =
  [ style "position" "absolute"
  , style "top" "1px"
  , style "left" "-27px"
  , style "background-color" "transparent"
  , style "border" "none"
  ]


-- Buttons

viewTextButton : String -> Msg -> Bool -> Html Msg
viewTextButton label msg isEnabled =
  button
    ( [ onClick msg
      , disabled <| not isEnabled
      , U.stopPropagationOnMousedown NoOp
      ]
      ++ textButtonStyle
    )
    [ text label ]


viewMapButton : String -> String -> Msg -> Bool -> Html Msg
viewMapButton label icon msg isEnabled =
  viewIconButton label icon C.mapToolbarIconSize msg isEnabled


viewItemButton : String -> String -> Msg -> Bool -> Html Msg
viewItemButton label icon msg isEnabled =
  viewIconButton label icon C.itemToolbarIconSize msg isEnabled


viewIconButton : String -> String -> Float -> Msg -> Bool -> Html Msg
viewIconButton label icon iconSize msg isEnabled =
  button
    ( [ class "tool"
      , title label
      , onClick msg
      , disabled <| not isEnabled
      , U.stopPropagationOnMousedown NoOp
      ]
      ++ iconButtonStyle
    )
    [ IconAPI.viewIcon icon iconSize ]


textButtonStyle : List (Attribute Msg)
textButtonStyle =
  [ style "font-family" C.mainFont
  , style "font-size" <| fromInt C.toolFontSize ++ "px"
  ]


iconButtonStyle : List (Attribute Msg)
iconButtonStyle =
  [ style "border" "none"
  , style "margin" "0 2px"
  , style "background-color" "transparent"
  ]



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
    ( newModel, topicId ) = Item.addTopic C.initTopicText Nothing model
    props = TopicV <| TopicProps
      ( Box.initTopicPos boxId model )
      C.topicDetailSize
      ( TopicD LabelOnly )
  in
  newModel
  |> Box.addItem topicId props boxId
  |> SelAPI.select topicId [ boxId ]


-- TODO: factor out addTopic() common code
addBox : Model -> Model
addBox model =
  let
    boxId = Box.active model
    ( newModel, topicId ) = Item.addTopic C.initBoxText Nothing model
    props = TopicV <| TopicProps
      ( Box.initTopicPos boxId model )
      C.topicDetailSize
      ( BoxD BlackBox )
  in
  newModel
  |> Box.addBox topicId
  |> Box.addItem topicId props boxId
  |> SelAPI.select topicId [ boxId ]


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
  |> SelAPI.clear
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
  |> SelAPI.clear
  |> Size.auto
