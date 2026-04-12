module Feature.Tool exposing (viewGlobalTools, viewMapTools, viewToolbar, viewTopicTools,
  closeMenu, update)

import Box
import Config as C
import ExtensionDef exposing (AutoSize)
import Feature.Icon as Icon
import Feature.Mouse as Mouse
import Feature.Nav as Nav
import Feature.Search as Search
import Feature.Sel as Sel
import Feature.Text as Text
import Feature.ToolDef as ToolDef exposing (LineStyle(..))
import Item
import Model exposing (Model, Msg(..))
import ModelBase exposing (..)
import Size
import Storage as S
import TopicMap.TopicMap as TM
import TopicMap.TopicMapDef exposing (ItemProps(..), TopicProps)
import Undo exposing (UndoModel)
import Utils as U

import Html exposing (Html, div, span, text, button, input, label, select, option)
import Html.Attributes exposing (class, style, title, name, value, type_, disabled, checked)
import Html.Events exposing (onClick, on, targetValue)
import Json.Decode as D
import String exposing (fromInt)



-- VIEW


-- Global Tools

viewGlobalTools : Model -> List (Html Msg)
viewGlobalTools model =
  let
    isAtHome = model.boxId == rootBoxId
    iconSize = C.globalToolsIconSize
  in
  [ viewIconButton "Show Home Map" "home" iconSize ToolDef.Home isAtHome Nothing homeButtonStyle
  , Search.viewInput model
  , viewIconButton "Settings" "menu" iconSize ToolDef.Menu False Nothing homeButtonStyle
  ]
  ++ viewMenu model


homeButtonStyle : Attrs Msg
homeButtonStyle =
  [ style "position" "relative"
  , style "top" "1px"
  ]


viewMenu : Model -> List (Html Msg)
viewMenu model =
  let
    is = (==) model.tool.lineStyle
  in
  if model.tool.menu then
    [ div
        menuStyle
        [ div headingStyle [ text "Line Style" ]
        , div
            []
            [ viewRadioButton "Cornered" (ToolDef.Set Cornered) <| is Cornered
            , hGap 20
            , viewRadioButton "Straight" (ToolDef.Set Straight) <| is Straight
            ]
        , vGap 32
        , div headingStyle [ text "Database" ]
        , div
            []
            [ viewTextButton "Import" ToolDef.Import
            , hGap 20
            , viewTextButton "Export" ToolDef.Export
            ]
        ]
    ]
  else
    []


menuStyle : Attrs Msg
menuStyle =
  [ style "font-size" <| fromInt C.toolFontSize ++ "px"
  , style "position" "absolute"
  , style "top" "32px"
  , style "right" "10px"
  , style "border" "1px solid lightgray"
  , style "background-color" "white"
  , style "padding" "24px 18px 16px"
  , style "z-index" "5"
  ]


viewRadioButton : String -> ToolDef.Msg -> Bool -> Html Msg
viewRadioButton label_ msg isChecked  =
  label
    [ U.onMouseDownStop NoOp ]
    [ input
      ( [ type_ "radio"
        , name "line-style"
        , checked isChecked
        , onClick <| Tool msg
        ]
        ++ radioButtonStyle
      )
      []
    , text label_
    ]


viewTextButton : String -> ToolDef.Msg -> Html Msg
viewTextButton label msg =
  button
    ( [ onClick <| Tool msg
      , U.onMouseDownStop NoOp
      ]
      ++ textButtonStyle
    )
    [ text label ]


headingStyle : Attrs Msg
headingStyle =
  [ style "font-weight" "bold"
  , style "margin-bottom" "14px"
  ]


radioButtonStyle : Attrs Msg
radioButtonStyle =
  [ style "margin" "0 6px 0 0" ]


textButtonStyle : Attrs Msg
textButtonStyle =
  [ style "font-family" C.mainFont
  , style "font-size" <| fromInt C.toolFontSize ++ "px"
  ]


hGap : Int -> Html Msg
hGap gap =
  span
    [ style "display" "inline-block"
    , style "width" <| fromInt gap ++ "px"
    ]
    []


vGap : Int -> Html Msg
vGap gap =
  div
    [ style "height" <| fromInt gap ++ "px" ]
    []


-- Map Tools

viewMapTools : UndoModel -> List (Html Msg)
viewMapTools undoModel =
  let
    target = TM.landingTarget undoModel.present
  in
  [ div
      mapToolsStyle
      -- The create-buttons must not clear the selection but still close menus.
      -- So we set the box where created things land as the target.
      [ viewMapButton "New Topic" "plus-circle" ToolDef.CreateTopic False target
      , viewMapButton "New Box" "plus-square" ToolDef.CreateBox False target
      , hGap 14
      , viewMapButton "Undo" "rotate-ccw" ToolDef.Undo (not <| Undo.hasPast undoModel) Nothing
      , viewMapButton "Redo" "rotate-cw" ToolDef.Redo (not <| Undo.hasFuture undoModel) Nothing
      ]
  ]


mapToolsStyle : Attrs Msg
mapToolsStyle =
  [ style "position" "fixed"
  , style "bottom" "20px"
  , style "left" "4px"
  , style "display" "flex"
  , style "gap" "4px"
  ]


-- Item Tools

-- Topic/Assoc toolbar, rendered by Map.view as box children
viewToolbar : BoxPath -> Extensions -> Model -> List (Html Msg)
viewToolbar boxPath allExt model =
  let
    boxId = Box.firstId boxPath
  in
  case Sel.single model of
    Just (itemId, selBoxPath) ->
      if selBoxPath == boxPath then
        case (Item.byId itemId model, TM.byId boxId model) of
          (Just {info}, Just {rect}) ->
            case info of
              Topic topic ->
                case TM.topicPos itemId boxId model of
                  Just topicPos ->
                    let
                      pos = Point
                        (topicPos.x - rect.x1 - C.topicW2)
                        (topicPos.y - rect.y1 - C.topicH2 - 29) -- TODO: 29 ≈ toolbar height
                    in
                    case (Text.isEdit itemId boxPath model, Item.isBox itemId model) of
                      (False, _) -> [ viewTopicToolbar pos itemId boxPath allExt model ]
                      (True, False) -> [ viewTextToolbar pos itemId boxPath ]
                      _ -> []
                  Nothing -> []
              Assoc assoc ->
                case TM.assocGeometry assoc boxId model of
                  Just (p1, p2) ->
                    let
                      pos = Point
                        ((p1.x + p2.x) // 2 - rect.x1 - 32) -- TODO: 32 ≈ toolbar width / 2
                        ((p1.y + p2.y) // 2 - rect.y1 - 13) -- TODO: 13 ≈ toolbar height / 2
                    in
                    [ viewAssocToolbar pos itemId boxPath ]
                  Nothing -> []
          _ -> []
      else
        []
    Nothing -> []


viewTopicToolbar : Point -> Id -> BoxPath -> Extensions -> Model -> Html Msg
viewTopicToolbar pos topicId boxPath allExt model =
  let
    target = (topicId, boxPath)
    topicTools =
      [ viewButton "Edit" "edit-3" ToolDef.Edit False target
      , viewButton "Select Icon" "smile" ToolDef.Icon False target
      , viewButton "Traverse" "share-2" ToolDef.Traverse False target
      , viewButton "Delete" "trash" ToolDef.Delete False target
      , viewButton "Remove" "x" ToolDef.Remove False target
      ]
    boxTools =
      if Item.isBox topicId model then
        let
          rendererSelect =
            case Box.rendererOf topicId model of
              Just renderer ->
                [ viewRendererSelect renderer (Tool << ToolDef.RendererSelected) allExt ]
              Nothing -> []
        in
        [ viewButton "Fullscreen" "maximize-2" (ToolDef.Fullscreen topicId) False target
        ]
        ++ rendererSelect
      else
        []
  in
  div
    ( toolbarStyle pos )
    ( topicTools
      ++ boxTools
      ++ Icon.viewPicker model
      ++ Search.viewTraversalResult model
    )


viewRendererSelect : Renderer -> (Renderer -> Msg) -> Extensions -> Html Msg
viewRendererSelect renderer toMsg allExt =
  select
    ( [ value renderer
      , on "input" (D.map toMsg targetValue)
      , U.onMouseDownStop NoOp
      ]
      ++ selectStyle
    )
    ( viewRendererOptions allExt )


viewRendererOptions : Extensions -> List (Html Msg)
viewRendererOptions allExt =
  allExt
    |> List.map
      (\(name, label) ->
        option
          [ value name ]
          [ text label ]
      )


selectStyle : Attrs Msg
selectStyle =
  [ style "position" "relative"
  , style "top" "-2px"
  , style "left" "3px"
  , style "font-family" C.mainFont
  , style "font-size" <| fromInt C.contentFontSize ++ "px"
  ]


viewTextToolbar : Point -> Id -> BoxPath -> Html Msg
viewTextToolbar pos topicId boxPath =
  let
    target = (topicId, boxPath)
  in
  div
    ( toolbarStyle pos )
    [ viewButton "Insert Image" "image" (ToolDef.Image topicId) False target
    , viewButton "Done" "check" ToolDef.LeaveEdit False target
    ]


viewAssocToolbar : Point -> Id -> BoxPath -> Html Msg
viewAssocToolbar pos assocId boxPath =
  let
    target = (assocId, boxPath)
  in
  div
    ( toolbarStyle pos )
    [ viewButton "Delete" "trash" ToolDef.Delete False target
    , viewButton "Remove" "x" ToolDef.Remove False target
    ]


toolbarStyle : Point -> Attrs Msg
toolbarStyle pos =
  [ style "position" "absolute"
  , style "top" <| fromInt pos.y ++ "px"
  , style "left" <| fromInt pos.x ++ "px"
  , style "display" "flex"
  , style "gap" "4px"
  , style "background-color" C.toolbarColor
  , style "border-radius" <| fromInt C.topicRadius ++ "px"
  , style "padding" "4px 5px 0"
  , style "z-index" "4"
  ]


-- Extra topic-specific tools, rendered by Map.view as topic children
-- TODO: remove this "hook" and render caret as box child
viewTopicTools : Id -> BoxPath -> Model -> List (Html Msg)
viewTopicTools topicId boxPath model =
  let
    boxId = Box.firstId boxPath
    isHovered = Mouse.isHovered topicId boxId model -- TODO: use boxPath
    isDrag = Mouse.isDragInProgress model
    isEdit = Text.isEdit topicId boxPath model
  in
  if isHovered && not isDrag && not isEdit then
    [ viewCaret topicId boxId model ]
  else
    []


viewCaret : Id -> BoxId -> Model -> Html Msg
viewCaret topicId boxId model =
  let
    icon =
      case Box.displayMode topicId boxId model of
        Just (TopicD LabelOnly) -> "chevron-right"
        Just (TopicD Detail) -> "chevron-down"
        Just (BoxD BlackBox) -> "chevron-right"
        Just (BoxD WhiteBox) -> "chevron-down"
        Nothing -> "??"
  in
  button
    ( [ onClick <| Tool <| ToolDef.ToggleDisplay topicId boxId
      , U.onMouseDownStop NoOp -- prevent cancel UI
      ]
      ++ caretStyle
    )
    [ Icon.view icon C.caretIconSize [] ]


caretStyle : Attrs Msg
caretStyle =
  [ style "position" "absolute"
  , style "top" "2px"
  , style "left" "-26px"
  , style "background-color" "transparent"
  , style "border" "none"
  ]


-- Icon Buttons

viewButton : String -> String -> ToolDef.Msg -> Bool -> Target -> Html Msg
viewButton label icon msg isDisabled target =
  viewIconButton label icon C.toolbarIconSize msg isDisabled (Just target) []


viewMapButton : String -> String -> ToolDef.Msg -> Bool -> Maybe Target -> Html Msg
viewMapButton label icon msg isDisabled maybeTarget =
  viewIconButton label icon C.mapToolbarIconSize msg isDisabled maybeTarget []


viewIconButton : String -> String -> Int -> ToolDef.Msg -> Bool -> Maybe Target -> Attrs Msg
                                                                                     -> Html Msg
viewIconButton label icon iconSize msg isDisabled maybeTarget extraStyle =
  button
    ( [ class "tool"
      , title label
      , onClick <| Tool msg
      , disabled isDisabled
      , U.onMouseDownStop <| Cancel maybeTarget
      ]
      ++ iconButtonStyle
      ++ extraStyle
    )
    [ Icon.view icon iconSize [] ]


iconButtonStyle : Attrs Msg
iconButtonStyle =
  [ style "border" "none"
  , style "background-color" "transparent"
  ]



-- UPDATE


update : ToolDef.Msg -> AutoSize -> UndoModel -> (UndoModel, Cmd Msg)
update msg autoSize ({present} as undoModel) =
  case msg of
    -- Global Tools
    ToolDef.Home -> (undoModel, Nav.pushUrl rootBoxId)
    ToolDef.Menu -> (openMenu present, Cmd.none) |> Undo.swap undoModel
    ToolDef.Set lineStyle -> setLineStyle lineStyle present |> S.store |> Undo.push undoModel
    ToolDef.Import -> (present, S.importJSON ()) |> Undo.swap undoModel
    ToolDef.Export -> (present, S.exportJSON ()) |> Undo.swap undoModel
    -- Map Tools
    ToolDef.CreateTopic -> createTopic autoSize present |> S.storeWith |> Undo.push undoModel
    ToolDef.CreateBox -> createBox autoSize present |> S.storeWith |> Undo.push undoModel
    ToolDef.Undo -> undoModel |> Undo.undo |> store
    ToolDef.Redo -> undoModel |> Undo.redo |> store
    -- Item Tools
    ToolDef.Edit -> edit autoSize present |> S.storeWith |> Undo.push undoModel
    ToolDef.Icon -> (Icon.openPicker present, Cmd.none) |> Undo.swap undoModel
    ToolDef.Traverse -> (Search.traverse present, Cmd.none) |> Undo.swap undoModel
    ToolDef.Delete -> delete autoSize present |> S.store |> Undo.push undoModel
    ToolDef.Remove -> remove autoSize present |> S.store |> Undo.push undoModel
    ToolDef.Fullscreen boxId -> (undoModel, Nav.pushUrl boxId)
    ToolDef.RendererSelected renderer -> setRenderer renderer autoSize present |> S.store
      |> Undo.swap undoModel
    ToolDef.ToggleDisplay topicId boxId -> toggleDisplay topicId boxId autoSize present
      |> S.store |> Undo.swap undoModel
    -- Text Tools
    ToolDef.Image topicId -> Text.openImageFilePicker topicId present |> S.storeWith
      |> Undo.swap undoModel
    ToolDef.LeaveEdit -> Text.leaveEdit autoSize present |> Undo.swap undoModel


-- Global Tools

openMenu : Model -> Model
openMenu model =
  setMenu True model


closeMenu : Model -> Model
closeMenu model =
  setMenu False model


setMenu : Bool -> Model -> Model
setMenu isOpen ({tool} as model) =
  { model | tool = { tool | menu = isOpen }}


setLineStyle : ToolDef.LineStyle -> Model -> Model
setLineStyle lineStyle ({tool} as model) =
  { model | tool = { tool | lineStyle = lineStyle }}


-- Map Tools

createTopic : AutoSize -> Model -> (Model, Cmd Msg)
createTopic autoSize model =
  let
    (newModel, topicId) = Item.createTopic "" C.initTopicIcon model
  in
  landTopic topicId (TopicD LabelOnly) autoSize newModel


createBox : AutoSize -> Model -> (Model, Cmd Msg)
createBox autoSize model =
  let
    (newModel, boxId) = Box.create "" C.initBoxIcon model
  in
  newModel
    |> TM.create boxId
    |> landTopic boxId (BoxD BlackBox) autoSize


landTopic : Id -> DisplayMode -> AutoSize -> Model -> (Model, Cmd Msg)
landTopic topicId displayMode autoSize model =
  let
    boxPath = Sel.landingBoxPath model
    boxId = Box.firstId boxPath
    props = TopicP <| TopicProps
      ( TM.initTopicPos boxId model )
      displayMode
  in
  model
    |> Box.addItem (ItemProps topicId displayMode) boxId
    |> TM.addItem topicId props boxId
    |> Sel.select topicId boxPath
    |> Text.enterEdit topicId boxPath autoSize


store : UndoModel -> (UndoModel, Cmd Msg)
store undoModel =
  ( undoModel, undoModel.present |> S.storeCmd )


-- Item Tools

edit : AutoSize -> Model -> (Model, Cmd Msg)
edit autoSize model =
  case Sel.single model of
    Just (topicId, boxPath) -> Text.enterEdit topicId boxPath autoSize model
    Nothing -> U.logError "edit" "called when there is no single selection" (model, Cmd.none)


delete : AutoSize -> Model -> Model
delete autoSize model =
  model.selection.items
    |> List.map Tuple.first
    |> List.foldr Box.deleteItem model
    |> Sel.clear
    |> Size.auto autoSize


remove : AutoSize -> Model -> Model
remove autoSize model =
  model.selection.items
    |> List.foldr
      (\(itemId, boxPath) modelAcc -> TM.removeItem itemId (Box.firstId boxPath) modelAcc)
      model
    |> Sel.clear
    |> Size.auto autoSize


setRenderer : Renderer -> AutoSize -> Model -> Model
setRenderer renderer autoSize model =
  case Sel.single model of
    Just (topicId, _) ->
      model
        |> Box.setRenderer topicId renderer
        |> Size.auto autoSize
    Nothing -> U.logError "setRenderer" "called when there is no single selection" model


toggleDisplay : Id -> BoxId -> AutoSize -> Model -> Model
toggleDisplay topicId boxId autoSize model =
  let
    (newModel, newDisplayMode) =
      case Box.displayMode topicId boxId model of
        Just (TopicD LabelOnly) -> (model, Just <| TopicD Detail)
        Just (TopicD Detail) -> (model, Just <| TopicD LabelOnly)
        Just (BoxD BlackBox) -> (model, Just <| BoxD WhiteBox)
        Just (BoxD WhiteBox) -> (model, Just <| BoxD BlackBox)
        Nothing -> (model, Nothing)
  in
  case (newModel, newDisplayMode) of
    (newModel_, Just displayMode) ->
      newModel_
        |> Box.setDisplayMode topicId boxId displayMode
        |> Size.auto autoSize
    _ -> model
