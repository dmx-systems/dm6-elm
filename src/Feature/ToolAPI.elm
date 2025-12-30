module Feature.ToolAPI exposing (viewGlobalTools, viewMapTools, viewItemTools, update)

import Box
import Box.Size as Size
import Box.Transfer as Transfer
import Config as C
import Feature.IconAPI as IconAPI
import Feature.MouseAPI as MouseAPI
import Feature.NavAPI as NavAPI
import Feature.SearchAPI as SearchAPI
import Feature.SelAPI as SelAPI
import Feature.TextAPI as TextAPI
import Feature.Tool as Tool
import Item
import Model exposing (Model, Msg(..))
import ModelParts exposing (..)
import Storage as S
import Undo exposing (UndoModel)
import Utils as U

import Html exposing (Html, div, span, text, button)
import Html.Attributes exposing (class, style, title, disabled)
import Html.Events exposing (onClick)
import String exposing (fromInt)



-- VIEW


-- Global Tools

viewGlobalTools : Model -> List (Html Msg)
viewGlobalTools model =
  let
    isHome = model.boxId == rootBoxId
  in
  [ viewIconButton "Show Home Map" "home" 20 (Tool Tool.Home) isHome True homeButtonStyle
  , SearchAPI.viewInput model
  , div
    importExportStyle
    [ viewTextButton "Import" (Tool Tool.Import)
    , viewTextButton "Export" (Tool Tool.Export)
    ]
  ]


homeButtonStyle : Attributes Msg
homeButtonStyle =
  [ style "position" "relative"
  , style "top" "1px"
  , style "left" "8px"
  ]


importExportStyle : Attributes Msg
importExportStyle =
  [ style "white-space" "nowrap" ]


-- Map Tools

viewMapTools : UndoModel -> List (Html Msg)
viewMapTools undoModel =
  [ div
    mapToolsStyle
    [ viewMapButton "Add Topic" "plus-circle" (Tool Tool.AddTopic) False
    , viewMapButton "Add Box" "plus-square" (Tool Tool.AddBox) False
    , viewSpacer
    , viewMapButton "Undo" "rotate-ccw" (Tool Tool.Undo) (not <| Undo.hasPast undoModel)
    , viewMapButton "Redo" "rotate-cw" (Tool Tool.Redo) (not <| Undo.hasFuture undoModel)
    ]
  ]


mapToolsStyle : Attributes Msg
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
        if TextAPI.isEdit itemId boxPath model then
          if Item.isBox itemId model then
            []
          else
            [ viewTextToolbar itemId boxId model ]
        else
          [ viewToolbar itemId boxId model ]
      else
        []
    caret =
      case MouseAPI.isHovered itemId boxId model of
        True -> viewCaret itemId boxId model
        False -> []
  in
  toolbar ++ caret


viewToolbar : Id -> BoxId -> Model -> Html Msg
viewToolbar itemId boxId model =
  let
    topicTools =
      [ viewItemButton "Edit" "edit-3" (Tool Tool.Edit) False True
      , viewItemButton "Select Icon" "smile" (Tool Tool.Icon) False True
      , viewItemButton "Traverse" "share-2" (Tool Tool.Traverse) False True
      , viewItemButton "Delete" "trash" (Tool Tool.Delete) False True
      , viewItemButton "Remove" "x" (Tool Tool.Remove) False True
      ]
    boxTools =
      if Item.isBox itemId model then
        let
          disabled = Box.isEmpty itemId model || Box.isUnboxed itemId boxId model
        in
        [ viewSpacer
        , viewItemButton "Fullscreen" "maximize-2" (Tool <| Tool.Fullscreen itemId) False True
        , viewItemButton "Unbox" "external-link" (Tool <| Tool.Unbox itemId boxId) disabled True
        ]
      else
        []
  in
  div
    ( toolbarStyle itemId boxId model )
    ( topicTools
      ++ boxTools
      ++ IconAPI.viewPicker model
      ++ SearchAPI.viewTraversalResult model
    )


-- Text Tools

viewTextToolbar : Id -> BoxId -> Model -> Html Msg
viewTextToolbar itemId boxId model =
  div
    ( toolbarStyle itemId boxId model )
    [ viewItemButton "Insert Image" "image" (Tool <| Tool.Image itemId) False False
    , viewItemButton "Done" "check" (Tool Tool.LeaveEdit) False False
    ]


--

toolbarStyle : Id -> BoxId -> Model -> Attributes Msg
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


viewCaret : Id -> BoxId -> Model -> List (Html Msg)
viewCaret itemId boxId model =
  if  Item.isBox itemId model && Box.isEmpty itemId model then
    []
  else
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
    [ button
        ( [ onClick <| Tool <| Tool.ToggleDisplay itemId boxId
          , U.onMouseDownStop NoOp -- prevent cancel UI
          ]
          ++ caretStyle
        )
        [ IconAPI.view icon 20 [] ]
    ]


caretStyle : Attributes Msg
caretStyle =
  [ style "position" "absolute"
  , style "top" "1px"
  , style "left" "-27px"
  , style "background-color" "transparent"
  , style "border" "none"
  ]


-- Buttons

viewTextButton : String -> Msg -> Html Msg
viewTextButton label msg =
  button
    ( [ onClick msg ]
      ++ textButtonStyle
    )
    [ text label ]


textButtonStyle : Attributes Msg
textButtonStyle =
  [ style "font-family" C.mainFont
  , style "font-size" <| fromInt C.toolFontSize ++ "px"
  ]


viewMapButton : String -> String -> Msg -> Bool -> Html Msg
viewMapButton label icon msg isDisabled =
  viewIconButton label icon C.mapToolbarIconSize msg isDisabled True []


viewItemButton : String -> String -> Msg -> Bool -> Bool -> Html Msg
viewItemButton label icon msg isDisabled shouldCancel =
  viewIconButton label icon C.itemToolbarIconSize msg isDisabled shouldCancel []


viewIconButton : String -> String -> Int -> Msg -> Bool -> Bool -> Attributes Msg -> Html Msg
viewIconButton label icon iconSize msg isDisabled shouldCancel extraStyle =
  let
    stop =
      case shouldCancel of
        True -> []
        False -> [ U.onMouseDownStop NoOp ]
  in
  button
    ( [ class "tool"
      , title label
      , onClick msg
      , disabled isDisabled
      ]
      ++ stop
      ++ iconButtonStyle
      ++ extraStyle
    )
    [ IconAPI.view icon iconSize [] ]


iconButtonStyle : Attributes Msg
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
    Tool.Home -> (undoModel, NavAPI.pushUrl rootBoxId)
    Tool.Import -> (present, S.importJSON ()) |> Undo.swap undoModel
    Tool.Export -> (present, S.exportJSON ()) |> Undo.swap undoModel
    -- Map Tools
    Tool.AddTopic -> addTopic present |> S.storeWith |> Undo.push undoModel
    Tool.AddBox -> addBox present |> S.storeWith |> Undo.push undoModel
    Tool.Undo -> Undo.undo undoModel
    Tool.Redo -> Undo.redo undoModel
    -- Item Tools
    Tool.Edit -> edit present |> S.storeWith |> Undo.push undoModel
    Tool.Icon -> (IconAPI.openPicker present, Cmd.none) |> Undo.swap undoModel
    Tool.Traverse -> (SearchAPI.traverse present, Cmd.none) |> Undo.swap undoModel
    Tool.Delete -> delete present |> S.store |> Undo.push undoModel
    Tool.Remove -> remove present |> S.store |> Undo.push undoModel
    Tool.Fullscreen boxId -> (undoModel, NavAPI.pushUrl boxId)
    Tool.Unbox boxId targetBoxId -> unbox boxId targetBoxId present |> S.store
      |> Undo.swap undoModel
    Tool.ToggleDisplay topicId boxId -> toggleDisplay topicId boxId present |> S.store
      |> Undo.swap undoModel
    -- Text Tools
    Tool.Image topicId -> TextAPI.openImageFilePicker topicId present |> S.storeWith
      |> Undo.swap undoModel
    Tool.LeaveEdit -> TextAPI.leaveEdit present |> Undo.swap undoModel


addTopic : Model -> (Model, Cmd Msg)
addTopic model =
  let
    boxId = model.boxId
    ( newModel, topicId ) = Item.addTopic C.initTopicText C.initTopicIcon model
    props = TopicP <| TopicProps
      ( Box.initTopicPos boxId model )
      ( TopicD LabelOnly )
  in
  newModel
  |> Box.addItem topicId props boxId
  |> SelAPI.select topicId [ boxId ]
  |> TextAPI.enterEdit topicId [ boxId ]


-- TODO: factor out addTopic() common code
addBox : Model -> (Model, Cmd Msg)
addBox model =
  let
    boxId = model.boxId
    ( newModel, topicId ) = Item.addTopic C.initBoxText C.initBoxIcon model
    props = TopicP <| TopicProps
      ( Box.initTopicPos boxId model )
      ( BoxD BlackBox )
  in
  newModel
  |> Box.addBox topicId
  |> Box.addItem topicId props boxId
  |> SelAPI.select topicId [ boxId ]
  |> TextAPI.enterEdit topicId [ boxId ]


edit : Model -> (Model, Cmd Msg)
edit model =
  case SelAPI.single model of
    Just (topicId, boxPath) -> TextAPI.enterEdit topicId boxPath model
    Nothing -> U.logError "edit" "called when there is no single selection" (model, Cmd.none)


delete : Model -> Model
delete model =
  let
    newModel = model.selection.items
      |> List.map Tuple.first
      |> List.foldr
        (\itemId modelAcc -> Box.deleteItem itemId modelAcc)
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


unbox : BoxId -> BoxId -> Model -> Model
unbox boxId targetBoxId model =
  Transfer.unboxContent boxId targetBoxId model
  |> Box.setDisplayMode boxId targetBoxId (BoxD Unboxed)
  |> Size.auto


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
