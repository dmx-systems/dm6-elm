module Feature.Tool exposing (ToolbarPos, viewGlobalTools, viewMapTools, viewToolbar,
  viewTopicTools, closeMenu, update)

import Assoc
import Box
import Config as C
import Env exposing (ExtManager, Env, Env2)
import Extension exposing (Renderer)
import Feature.Icon as Icon
import Feature.Mouse as Mouse
import Feature.Nav as Nav
import Feature.Search as Search
import Feature.Sel as Sel
import Feature.Text as Text
import Feature.ToolDef as ToolDef exposing (LineStyle(..))
import Model exposing (Model, Msg(..))
import ModelBase exposing (..)
import Shared.Events as Events
import Storage as S
import Topic
import TopicMap.TopicMap as TopicMap
import Undo exposing (UndoModel)
import Utils as U

import Html exposing (Html, div, span, text, button, input, label, select, option)
import Html.Attributes exposing (class, style, title, name, value, type_, disabled, checked)
import Html.Events exposing (onClick, on, targetValue)
import Json.Decode as D
import String exposing (fromInt)



-- MODEL


type alias ToolbarPos =
  { topic : Topic -> Point
  , assoc : Assoc -> Point
  }



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
    [ Events.onPointerDownStop NoOp ]
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
      , Events.onPointerDownStop NoOp
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
    target = TopicMap.landingTarget undoModel.present -- FIXME: dispatch via ExtManager
  in
  [ div
      mapToolsStyle
      -- The create-buttons must not clear the selection but still close menus.
      -- So we set the box where created things land as the target.
      [ viewMapButton "New Topic" "plus-circle" ToolDef.CreateTopic False target
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

-- Topic/Assoc toolbar, rendered by ExtManager.view as box children
viewToolbar : BoxPath -> ToolbarPos -> Env2 -> List (Html Msg)
viewToolbar boxPath toolbarPos ({model, ext} as env) =
  case Sel.single model of
    Just (itemId, selBoxPath) ->
      if selBoxPath == boxPath then
        case itemId of
          T id ->
            case Topic.fromId id model of
              Just topic ->
                let
                  pos = toolbarPos.topic topic
                in
                case (Text.isEdit id boxPath model, Topic.isBox id model) of
                  (False, _) -> [ viewTopicToolbar pos id boxPath env ]
                  (True, False) -> [ viewTextToolbar pos id boxPath ]
                  _ -> []
              Nothing -> []
          A id ->
            case Assoc.fromId id model of
              Just assoc ->
                let
                  pos = toolbarPos.assoc assoc
                in
                [ viewAssocToolbar pos assoc boxPath ]
              Nothing -> []
      else
        []
    Nothing -> []


viewTopicToolbar : Point -> TopicId -> BoxPath -> Env2 -> Html Msg
viewTopicToolbar pos topicId boxPath ({model, ext}) =
  let
    target = (T topicId, boxPath)
    topicTools =
      [ viewButton "Edit" "edit-3" ToolDef.Edit False target
      , viewButton "Select Icon" "smile" ToolDef.Icon False target
      , viewButton "Traverse" "share-2" ToolDef.Traverse False target
      , viewButton "Delete" "trash" ToolDef.Delete False target
      , viewButton "Remove" "x" ToolDef.Remove False target
      , viewButton "Fullscreen" "maximize-2" (ToolDef.Fullscreen topicId) False target
      ]
    boxTools =
      if Topic.isBox topicId model then
        case Box.rendererOf (BoxId topicId) model of
          Just renderer ->
            [ viewRendererSelect renderer (Tool << ToolDef.RendererSelected) ext ]
          Nothing -> []
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


viewRendererSelect : Renderer -> (Renderer -> Msg) -> ExtManager -> Html Msg
viewRendererSelect renderer toMsg ext =
  select
    ( [ value <| Extension.toString renderer
      , on "input" (targetValue |> Extension.rendererDecoder |> D.map toMsg)
      , Events.onPointerDownStop NoOp
      ]
      ++ selectStyle
    )
    ( viewRendererOptions ext )


viewRendererOptions : ExtManager -> List (Html Msg)
viewRendererOptions ext =
  ext.all
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


viewTextToolbar : Point -> TopicId -> BoxPath -> Html Msg
viewTextToolbar pos topicId boxPath =
  let
    target = (T topicId, boxPath)
  in
  div
    ( toolbarStyle pos )
    [ viewButton "Insert Image" "image" (ToolDef.Image topicId) False target
    , viewButton "Done" "check" ToolDef.LeaveEdit False target
    ]


viewAssocToolbar : Point -> Assoc -> BoxPath -> Html Msg
viewAssocToolbar pos assoc boxPath =
  let
    target = (A assoc.id, boxPath)
  in
  div
    ( toolbarStyle pos )
    [ viewButton "Delete" "trash" ToolDef.Delete (assoc.assocType == Hierarchy) target
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
viewTopicTools : TopicId -> BoxPath -> Model -> List (Html Msg)
viewTopicTools topicId boxPath model =
  let
    boxId = Box.firstId boxPath
    isHovered = Mouse.isHovered topicId boxPath model
    isDrag = Mouse.isDragActive model
    isEdit = Text.isEdit topicId boxPath model
  in
  if isHovered && not isDrag && not isEdit then
    [ viewCaret topicId boxId model ]
  else
    []


viewCaret : TopicId -> BoxId -> Model -> Html Msg
viewCaret topicId boxId model =
  let
    icon =
      case Box.expansionOf topicId boxId model of
        Collapsed -> "chevron-right"
        Expanded -> "chevron-down"
  in
  button
    ( [ onClick <| Tool <| ToolDef.ToggleExpansion topicId boxId
      , Events.onPointerDownStop NoOp -- prevent cancel UI
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
      , Events.onPointerDownStop <| Cancel maybeTarget
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


update : ToolDef.Msg -> Env -> (UndoModel, Cmd Msg)
update msg ({model, undoModel, ext} as env) =
  case msg of
    -- Global Tools
    ToolDef.Home -> (undoModel, Nav.pushUrl rootBoxId)
    ToolDef.Menu -> (openMenu model, Cmd.none) |> Undo.swap undoModel
    ToolDef.Set lineStyle -> setLineStyle lineStyle model |> S.store |> Undo.push undoModel
    ToolDef.Import -> (model, S.importJSON ()) |> Undo.swap undoModel
    ToolDef.Export -> (model, S.exportJSON ()) |> Undo.swap undoModel
    -- Map Tools
    ToolDef.CreateTopic -> createTopic env |> S.storeWith |> Undo.push undoModel
    ToolDef.Undo -> undoModel |> Undo.undo |> store
    ToolDef.Redo -> undoModel |> Undo.redo |> store
    -- Item Tools
    ToolDef.Edit -> edit env |> S.storeWith |> Undo.push undoModel
    ToolDef.Icon -> (Icon.openPicker model, Cmd.none) |> Undo.swap undoModel
    ToolDef.Traverse -> (Search.traverse model, Cmd.none) |> Undo.swap undoModel
    ToolDef.Delete -> delete env |> S.store |> Undo.push undoModel
    ToolDef.Remove -> remove env |> S.store |> Undo.push undoModel
    ToolDef.Fullscreen topicId -> fullscreen topicId (Env2 model ext) |> S.storeWith
      |> Undo.push undoModel
    ToolDef.RendererSelected renderer -> setRenderer renderer env |> S.store
      |> Undo.swap undoModel
    ToolDef.ToggleExpansion topicId boxId -> toggleExpansion topicId boxId env |> S.store
      |> Undo.swap undoModel
    -- Text Tools
    ToolDef.Image topicId -> Text.openImageFilePicker topicId model |> S.storeWith
      |> Undo.swap undoModel
    ToolDef.LeaveEdit -> Text.leaveEdit env |> Undo.swap undoModel


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

createTopic : Env -> (Model, Cmd Msg)
createTopic ({model} as env) =
  let
    (newModel, topicId) = Topic.create "" C.initTopicIcon model
    newEnv = Env.withModel env newModel
  in
  landTopic topicId newEnv


landTopic : TopicId -> Env -> (Model, Cmd Msg)
landTopic topicId ({model, ext} as env) =
  let
    boxPath = Sel.landingBoxPath model
    boxId = Box.firstId boxPath
  in
  Env2 model ext
    |> Box.addTopic (BoxTopic topicId Collapsed) boxId
    |> Sel.select (T topicId) boxPath
    |> Env.withModel env
    |> Text.enterEdit topicId boxPath


store : UndoModel -> (UndoModel, Cmd Msg)
store undoModel =
  ( undoModel, undoModel.present |> S.storeCmd )


-- Item Tools

edit : Env -> (Model, Cmd Msg)
edit ({model} as env) =
  case Sel.single model of
    Just (T id, boxPath) -> Text.enterEdit id boxPath env
    _ -> U.logError "Feature.Tool.edit" "called when there is no single topic selection"
      (model, Cmd.none)


delete : Env -> Model
delete ({model} as env) =
  model.selection.items
    |> List.map Tuple.first
    |> List.foldr delete_ model
    |> Sel.clear
    |> Env.autoSize env


delete_ : ItemId -> Model -> Model
delete_ itemId model =
  case itemId of
    T id -> Box.deleteTopic id model
    A id -> Box.deleteAssoc id model


remove : Env -> Model
remove ({model} as env) =
  model.selection.items
    |> List.foldr remove_ model
    |> Sel.clear
    |> Env.autoSize env


remove_ : (ItemId, BoxPath) -> Model -> Model
remove_ (itemId, boxPath) model =
  let
    boxId = (Box.firstId boxPath)
  in
  case itemId of
    T id -> Box.removeTopic id boxId model
    A id -> Box.removeAssoc id boxId model


fullscreen : TopicId -> Env2 -> (Model, Cmd Msg)
fullscreen topicId env =
  ( env
      |> Box.turnTopicIntoBox topicId Extension.defaultRenderer
  , Nav.pushUrl (BoxId topicId)
  )


setRenderer : Renderer -> Env -> Model
setRenderer renderer ({model, ext} as env) =
  case Sel.single model of
    Just (T topicId, _) ->
      let
        boxId = (BoxId topicId)
      in
      model
        |> Box.setRenderer boxId renderer
        |> Env.with2 ext
        |> Box.init boxId
        |> Env.autoSize env
    _ -> U.logError "Feature.Tool.setRenderer" "called when there is no single topic selection"
      model


toggleExpansion : TopicId -> BoxId -> Env -> Model
toggleExpansion topicId boxId ({model} as env) =
  model
    |> Box.updateExpansion topicId boxId  
        (\expansion ->
          case expansion of
            Collapsed -> Expanded
            Expanded -> Collapsed
        )
    |> Env.autoSize env
