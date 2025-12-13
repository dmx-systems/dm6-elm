port module Feature.ToolAPI exposing (viewGlobalTools, viewMapTools, viewItemTools, update)

import Box
import Box.Size as Size
import Box.Transfer as Transfer
import Config as C
import Feature.Icon as Icon
import Feature.IconAPI as IconAPI
import Feature.MouseAPI as MouseAPI
import Feature.NavAPI as NavAPI
import Feature.Search as Search
import Feature.SearchAPI as SearchAPI
import Feature.SelAPI as SelAPI
import Feature.TextEditAPI as TextEditAPI
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



-- PORTS


port filePicker : () -> Cmd msg



-- VIEW


-- Global Tools

viewGlobalTools : Model -> List (Html Msg)
viewGlobalTools model =
  let
    isHome = model.boxId == rootBoxId
  in
  [ viewIconButton "Show Home Map" "home" 20 (Tool Tool.Home) (not isHome) homeButtonStyle
  , SearchAPI.viewInput model
  , div
    importExportStyle
    [ viewTextButton "Import" (Tool Tool.Import) True
    , viewTextButton "Export" (Tool Tool.Export) True
    ]
  ]


homeButtonStyle : List (Attribute Msg)
homeButtonStyle =
  [ style "position" "relative"
  , style "top" "1px"
  , style "left" "8px"
  ]


importExportStyle : List (Attribute Msg)
importExportStyle =
  [ style "white-space" "nowrap" ]


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

viewItemTools : Id -> BoxPath -> Model -> List (Html Msg)
viewItemTools itemId boxPath model =
  let
    boxId = Box.firstId boxPath
    toolbar =
      if SelAPI.isSelectedPath itemId boxPath model then
        if TextEditAPI.isEdit itemId boxPath model then
          [ viewTextToolbar itemId boxId model ]
        else
          [ viewToolbar itemId boxId model ]
      else
        []
    caret =
      case MouseAPI.isHovered itemId boxId model of
        True -> [ viewCaret itemId boxId model ]
        False -> []
  in
  toolbar ++ caret


viewToolbar : Id -> BoxId -> Model -> Html Msg
viewToolbar itemId boxId model =
  let
    topicTools =
      [ viewItemButton "Edit" "edit-3" (Tool Tool.Edit) True
      , viewItemButton "Choose Icon" "image" (Icon Icon.OpenMenu) True
      , viewItemButton "Traverse" "share-2" (Search Search.Traverse) True
      , viewItemButton "Delete" "trash" (Tool Tool.Delete) True
      , viewItemButton "Remove" "x" (Tool Tool.Remove) True
      ]
    boxTools =
      if Item.isBox itemId model then
        [ viewSpacer
        , viewItemButton "Fullscreen" "maximize-2" (Tool <| Tool.Fullscreen itemId) True
        , viewItemButton "Unbox" "external-link" (Tool <| Tool.Unbox itemId boxId) (not unboxed)
        ]
      else
        []
    unboxed =
      Box.displayMode itemId boxId model == Just (BoxD Unboxed)
  in
  div
    ( toolbarStyle itemId boxId model )
    ( topicTools
      ++ boxTools
      ++ IconAPI.viewMenu model
      ++ SearchAPI.viewTraversalResult model
    )


-- Text Tools

viewTextToolbar : Id -> BoxId -> Model -> Html Msg
viewTextToolbar itemId boxId model =
  div
    ( toolbarStyle itemId boxId model )
    [ viewItemButton "Insert Image" "image" (Tool Tool.Image) True
    , viewItemButton "Insert Link" "link-2" (Tool Tool.Link) True
    ]


--

toolbarStyle : Id -> BoxId -> Model -> List (Attribute Msg)
toolbarStyle itemId boxId model =
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
  , style "z-index" "2"
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
    [ IconAPI.view icon 20 [] ]


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
  viewIconButton label icon C.mapToolbarIconSize msg isEnabled []


viewItemButton : String -> String -> Msg -> Bool -> Html Msg
viewItemButton label icon msg isEnabled =
  viewIconButton label icon C.itemToolbarIconSize msg isEnabled []


viewIconButton : String -> String -> Float -> Msg -> Bool -> List (Attribute Msg) -> Html Msg
viewIconButton label icon iconSize msg isEnabled extraStyle =
  button
    ( [ class "tool"
      , title label
      , onClick msg
      , disabled <| not isEnabled
      , U.stopPropagationOnMousedown NoOp
      ]
      ++ iconButtonStyle
      ++ extraStyle
    )
    [ IconAPI.view icon iconSize [] ]


textButtonStyle : List (Attribute Msg)
textButtonStyle =
  [ style "font-family" C.mainFont
  , style "font-size" <| fromInt C.toolFontSize ++ "px"
  ]


iconButtonStyle : List (Attribute Msg)
iconButtonStyle =
  [ style "border" "none"
  , style "background-color" "transparent"
  , style "margin" "0 2px"
  ]



-- UPDATE


update : Tool.Msg -> UndoModel -> (UndoModel, Cmd Msg)
update msg ({present} as undoModel) =
  case msg of
    -- Global Tools
    Tool.Home -> (undoModel, NavAPI.pushUrl rootBoxId present)
    Tool.Import -> (present, S.importJSON ()) |> Undo.swap undoModel
    Tool.Export -> (present, S.exportJSON ()) |> Undo.swap undoModel
    -- Map Tools
    Tool.AddTopic -> addTopic present |> S.storeWith |> Undo.push undoModel
    Tool.AddBox -> addBox present |> S.storeWith |> Undo.push undoModel
    Tool.Undo -> Undo.undo undoModel
    Tool.Redo -> Undo.redo undoModel
    -- Item Tools
    Tool.Edit -> edit present |> S.storeWith |> Undo.swap undoModel
    Tool.Delete -> delete present |> S.store |> Undo.push undoModel
    Tool.Remove -> remove present |> S.store |> Undo.push undoModel
    Tool.Fullscreen boxId -> (undoModel, NavAPI.pushUrl boxId present)
    Tool.Unbox boxId targetBoxId -> unbox boxId targetBoxId present |> S.store
      |> Undo.swap undoModel
    Tool.ToggleDisplay topicId boxId -> toggleDisplay topicId boxId present |> S.store
      |> Undo.swap undoModel
    -- Text Tools
    Tool.Image -> (undoModel, filePicker ())
    Tool.Link -> (undoModel, Cmd.none)


addTopic : Model -> (Model, Cmd Msg)
addTopic model =
  let
    boxId = model.boxId
    ( newModel, topicId ) = Item.addTopic C.initTopicText Nothing model
    props = TopicV <| TopicProps
      ( Box.initTopicPos boxId model )
      ( TopicD LabelOnly )
  in
  newModel
  |> Box.addItem topicId props boxId
  |> SelAPI.select topicId [ boxId ]
  |> TextEditAPI.startEdit topicId [ boxId ]


-- TODO: factor out addTopic() common code
addBox : Model -> (Model, Cmd Msg)
addBox model =
  let
    boxId = model.boxId
    ( newModel, topicId ) = Item.addTopic C.initBoxText Nothing model
    props = TopicV <| TopicProps
      ( Box.initTopicPos boxId model )
      ( BoxD BlackBox )
  in
  newModel
  |> Box.addBox topicId
  |> Box.addItem topicId props boxId
  |> SelAPI.select topicId [ boxId ]
  |> TextEditAPI.startEdit topicId [ boxId ]


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
          ( Transfer.boxContent topicId boxId model
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
  Transfer.unboxContent boxId targetBoxId model
  |> Box.setDisplayMode boxId targetBoxId (BoxD Unboxed)
  |> Size.auto


edit : Model -> (Model, Cmd Msg)
edit model =
  case SelAPI.single model of
    Just (topicId, boxPath) -> TextEditAPI.startEdit topicId boxPath model
    Nothing -> U.logError "edit" "called when there is no single selection" (model, Cmd.none)


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


remove : Model -> Model
remove model =
  let
    newModel = model.selection.items
      |> List.foldr
        (\(itemId, boxPath) modelAcc -> Box.removeItem itemId (Box.firstId boxPath) modelAcc)
        model
  in
  newModel
  |> SelAPI.clear
  |> Size.auto



insertImage : Model -> Model -- TODO
insertImage model =
  model
