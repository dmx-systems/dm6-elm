module Feature.ToolAPI exposing (viewGlobalTools, viewMapTools, viewToolbar, viewTopicTools,
  closeMenu, update)

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
import Feature.Tool as Tool exposing (LineStyle(..))
import Item
import Model exposing (Model, Msg(..))
import ModelParts exposing (..)
import Storage as S
import Undo exposing (UndoModel)
import Utils as U

import Html exposing (Html, div, span, text, button, input, label)
import Html.Attributes exposing (class, style, title, name, type_, disabled, checked)
import Html.Events exposing (onClick)
import String exposing (fromInt)



-- VIEW


-- Global Tools

viewGlobalTools : Model -> List (Html Msg)
viewGlobalTools model =
  let
    isHome = model.boxId == homeBoxId
    iconSize = C.globalToolsIconSize
  in
  [ viewIconButton "Show Home Map" "home" iconSize Tool.Home isHome Nothing homeButtonStyle
  , SearchAPI.viewInput model
  , viewIconButton "Settings" "menu" iconSize Tool.Menu False Nothing homeButtonStyle
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
            [ viewRadioButton "Cornered" (Tool.Set Cornered) <| is Cornered
            , hGap 20
            , viewRadioButton "Straight" (Tool.Set Straight) <| is Straight
            ]
        , vGap 32
        , div headingStyle [ text "Database" ]
        , div
            []
            [ viewTextButton "Import" Tool.Import
            , hGap 20
            , viewTextButton "Export" Tool.Export
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


viewRadioButton : String -> Tool.Msg -> Bool -> Html Msg
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


viewTextButton : String -> Tool.Msg -> Html Msg
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
  [ div
    mapToolsStyle
    [ viewMapButton "Add Topic" "plus-circle" Tool.AddTopic False
    , viewMapButton "Add Box" "plus-square" Tool.AddBox False
    , hGap 14
    , viewMapButton "Undo" "rotate-ccw" Tool.Undo (not <| Undo.hasPast undoModel)
    , viewMapButton "Redo" "rotate-cw" Tool.Redo (not <| Undo.hasFuture undoModel)
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
viewToolbar : BoxPath -> Model -> List (Html Msg)
viewToolbar boxPath model =
  let
    boxId = Box.firstId boxPath
  in
  case SelAPI.single model of
    Just (itemId, selBoxPath) ->
      if selBoxPath == boxPath then
        case (Item.byId itemId model, Box.byIdOrLog boxId model) of
          (Just {info}, Just {rect}) ->
            case info of
              Topic topic ->
                case Box.topicPos itemId boxId model of
                  Just topicPos ->
                    let
                      pos = Point
                        (topicPos.x - rect.x1 - C.topicW2)
                        (topicPos.y - rect.y1 - C.topicH2 - 29) -- TODO: 29 ≈ toolbar height
                    in
                    case (TextAPI.isEdit itemId boxPath model, Item.isBox itemId model) of
                      (False, _) -> [ viewTopicToolbar pos itemId boxPath model ]
                      (True, False) -> [ viewTextToolbar pos itemId boxPath ]
                      _ -> []
                  Nothing -> []
              Assoc assoc ->
                case Box.assocGeometry assoc boxId model of
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


viewTopicToolbar : Point -> Id -> BoxPath -> Model -> Html Msg
viewTopicToolbar pos topicId boxPath model =
  let
    boxId = Box.firstId boxPath
    target = (topicId, boxPath)
    topicTools =
      [ viewButton "Edit" "edit-3" Tool.Edit False target
      , viewButton "Select Icon" "smile" Tool.Icon False target
      , viewButton "Traverse" "share-2" Tool.Traverse False target
      , viewButton "Delete" "trash" Tool.Delete False target
      , viewButton "Remove" "x" Tool.Remove False target
      ]
    boxTools =
      if Item.isBox topicId model then
        let
          isDisabled = Box.isEmpty topicId model || Box.isUnboxed topicId boxId model
        in
        [ hGap 14
        , viewButton "Fullscreen" "maximize-2" (Tool.Fullscreen topicId) False target
        , viewButton "Unbox" "external-link" (Tool.Unbox topicId boxId) isDisabled target
        ]
      else
        []
  in
  div
    ( toolbarStyle pos )
    ( topicTools
      ++ boxTools
      ++ IconAPI.viewPicker model
      ++ SearchAPI.viewTraversalResult model
    )


viewTextToolbar : Point -> Id -> BoxPath -> Html Msg
viewTextToolbar pos topicId boxPath =
  let
    target = (topicId, boxPath)
  in
  div
    ( toolbarStyle pos )
    [ viewButton "Insert Image" "image" (Tool.Image topicId) False target
    , viewButton "Done" "check" Tool.LeaveEdit False target
    ]


viewAssocToolbar : Point -> Id -> BoxPath -> Html Msg
viewAssocToolbar pos assocId boxPath =
  let
    target = (assocId, boxPath)
  in
  div
    ( toolbarStyle pos )
    [ viewButton "Delete" "trash" Tool.Delete False target
    , viewButton "Remove" "x" Tool.Remove False target
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
viewTopicTools : Id -> BoxPath -> Model -> List (Html Msg)
viewTopicTools topicId boxPath model =
  let
    boxId = Box.firstId boxPath
    isHovered = MouseAPI.isHovered topicId boxId model -- TODO: use boxPath
    isEmptyBox = Item.isBox topicId model && Box.isEmpty topicId model
    isEdit = TextAPI.isEdit topicId boxPath model
  in
  if isHovered && not isEmptyBox && not isEdit then
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
        Just (BoxD Unboxed) -> "chevron-down"
        Nothing -> "??"
  in
  button
    ( [ onClick <| Tool <| Tool.ToggleDisplay topicId boxId
      , U.onMouseDownStop NoOp -- prevent cancel UI
      ]
      ++ caretStyle
    )
    [ IconAPI.view icon C.caretIconSize [] ]


caretStyle : Attrs Msg
caretStyle =
  [ style "position" "absolute"
  , style "top" "2px"
  , style "left" "-26px"
  , style "background-color" "transparent"
  , style "border" "none"
  ]


-- Icon Buttons

viewButton : String -> String -> Tool.Msg -> Bool -> (Id, BoxPath) -> Html Msg
viewButton label icon msg isDisabled target =
  viewIconButton label icon C.toolbarIconSize msg isDisabled (Just target) []


viewMapButton : String -> String -> Tool.Msg -> Bool -> Html Msg
viewMapButton label icon msg isDisabled =
  viewIconButton label icon C.mapToolbarIconSize msg isDisabled Nothing []


viewIconButton : String -> String -> Int -> Tool.Msg -> Bool -> Maybe (Id, BoxPath)
                                                                  -> Attrs Msg -> Html Msg
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
    [ IconAPI.view icon iconSize [] ]


iconButtonStyle : Attrs Msg
iconButtonStyle =
  [ style "border" "none"
  , style "background-color" "transparent"
  ]



-- UPDATE


update : Tool.Msg -> UndoModel -> (UndoModel, Cmd Msg)
update msg ({present} as undoModel) =
  case msg of
    -- Global Tools
    Tool.Home -> (undoModel, NavAPI.pushUrl homeBoxId)
    Tool.Menu -> (openMenu present, Cmd.none) |> Undo.swap undoModel
    Tool.Set lineStyle -> setLineStyle lineStyle present |> S.store |> Undo.push undoModel
    Tool.Import -> (present, S.importJSON ()) |> Undo.swap undoModel
    Tool.Export -> (present, S.exportJSON ()) |> Undo.swap undoModel
    -- Map Tools
    Tool.AddTopic -> addTopic present |> S.storeWith |> Undo.push undoModel
    Tool.AddBox -> addBox present |> S.storeWith |> Undo.push undoModel
    Tool.Undo -> undoModel |> Undo.undo |> store
    Tool.Redo -> undoModel |> Undo.redo |> store
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


setLineStyle : Tool.LineStyle -> Model -> Model
setLineStyle lineStyle ({tool} as model) =
  { model | tool = { tool | lineStyle = lineStyle }}


-- Map Tools

addTopic : Model -> (Model, Cmd Msg)
addTopic model =
  let
    boxId = model.boxId
    ( newModel, topicId ) = Item.addTopic "" C.initTopicIcon model
    props = TopicP <| TopicProps
      ( Box.initTopicPos boxId model )
      ( TopicD LabelOnly )
  in
  newModel
    |> Box.addItem topicId props boxId
    |> SelAPI.select topicId [ boxId ]
    |> TextAPI.enterEdit topicId [ boxId ]


-- TODO: factor out addTopic() common code?
addBox : Model -> (Model, Cmd Msg)
addBox model =
  let
    boxId = model.boxId
    ( newModel, topicId ) = Item.addTopic "" C.initBoxIcon model
    props = TopicP <| TopicProps
      ( Box.initTopicPos boxId model )
      ( BoxD BlackBox )
  in
  newModel
    |> Box.addBox topicId
    |> Box.addItem topicId props boxId
    |> SelAPI.select topicId [ boxId ]
    |> TextAPI.enterEdit topicId [ boxId ]


store : UndoModel -> (UndoModel, Cmd Msg)
store undoModel =
  ( undoModel, undoModel.present |> S.storeCmd )



-- Item Tools

edit : Model -> (Model, Cmd Msg)
edit model =
  case SelAPI.single model of
    Just (topicId, boxPath) -> TextAPI.enterEdit topicId boxPath model
    Nothing -> U.logError "edit" "called when there is no single selection" (model, Cmd.none)


delete : Model -> Model
delete model =
  model.selection.items
    |> List.map Tuple.first
    |> List.foldr Box.deleteItem model
    |> SelAPI.clear
    |> Size.auto


remove : Model -> Model
remove model =
  model.selection.items
    |> List.foldr
      (\(itemId, boxPath) modelAcc -> Box.removeItem itemId (Box.firstId boxPath) modelAcc)
      model
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
