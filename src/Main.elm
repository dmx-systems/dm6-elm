module Main exposing (..)

import AppModel exposing (..)
import Boxing exposing (boxContainer, unboxContainer)
import Config exposing (..)
import MapAutoSize exposing (autoSize)
import MapRenderer exposing (viewMap)
import Model exposing (..)
import ModelAPI exposing (..)
import Storage exposing (storeModel, storeModelWith, modelDecoder)
import Utils exposing (..)
-- components
import IconMenu exposing (IconMenuMsg(..))
import IconMenuAPI exposing (viewIcon, viewIconMenu, updateIconMenu)
import MouseAPI exposing (mouseHoverHandler, mouseSubs, updateMouse)
import SearchAPI exposing (viewSearchInput, viewResultMenu, updateSearch)

import Browser
import Browser.Dom as Dom
import Dict
import Html exposing (Html, Attribute, div, span, text, br, button, input, label, a)
import Html.Attributes exposing (id, href, style, type_, name, checked, disabled)
import Html.Events exposing (onClick)
import String exposing (fromInt, fromFloat)
import Task
import Json.Decode as D
import Json.Encode as E



-- MAIN


main : Program E.Value Model Msg
main =
  Browser.document
    { init = init
    , view = view
    , update = update
    , subscriptions = mouseSubs
    }


init : E.Value -> (Model, Cmd Msg)
init flags =
  ( case flags |> D.decodeValue (D.null True) of
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
  , Cmd.none
  )



-- VIEW


view : Model -> Browser.Document Msg
view model =
  Browser.Document
    "DM6 Elm"
    [ div
      ( mouseHoverHandler
        ++ appStyle
      )
      ( [ viewToolbar model
        , viewMap (activeMap model) -1 model -- parentMapId = -1
        ]
        ++ viewResultMenu model
        ++ viewIconMenu model
      )
    , div
      ( [ id "measure" ]
        ++ measureStyle
      )
      [ text model.measureText
      , br [] []
      ]
    ]


appStyle : List (Attribute Msg)
appStyle =
  [ style "font-family" mainFont
  , style "user-select" "none"
  , style "-webkit-user-select" "none" -- Safari still needs vendor prefix
  ]


viewToolbar : Model -> Html Msg
viewToolbar model =
  div
    toolbarStyle
    [ viewMapNav model
    , viewSearchInput model
    , viewToolbarButton "Add Topic" AddTopic False model
    , viewToolbarButton "Edit" (Edit EditStart) True model
    , viewToolbarButton "Choose Icon" (IconMenu Open) True model
    , viewMonadDisplay model
    , viewContainerDisplay model
    , viewToolbarButton "Hide" Hide True model
    , viewToolbarButton "Fullscreen" (Nav Fullscreen) True model
    , viewToolbarButton "Delete" Delete True model
    , viewFooter
    ]


toolbarStyle : List (Attribute Msg)
toolbarStyle =
  [ style "font-size" <| fromInt toolbarFontSize ++ "px"
  , style "display" "flex"
  , style "flex-direction" "column"
  , style "align-items" "flex-start"
  , style "gap" "28px"
  , style "position" "fixed"
  , style "z-index" "1"
  ]


viewMapNav : Model -> Html Msg
viewMapNav model =
  let
    backDisabled = isHome model
  in
  div
    mapNavStyle
    [ button
      [ onClick (Nav Back)
      , disabled backDisabled
      ]
      [ viewIcon "arrow-left" 20 ]
    , span
      mapTitleStyle
      [ text <| getMapName model ]
    ]


mapNavStyle : List (Attribute Msg)
mapNavStyle =
  [ style "margin-top" "20px"
  , style "margin-bottom" "12px"
  ]


mapTitleStyle : List (Attribute Msg)
mapTitleStyle =
  [ style "font-size" "36px"
  , style "font-weight" "bold"
  , style "vertical-align" "top"
  , style "margin-left" "12px"
  ]


getMapName : Model -> String
getMapName model =
  if isHome model then -- home map has no corresponding topic
    homeMapName
  else
    case getTopicInfo (activeMap model) model of
      Just topic -> getTopicLabel topic
      Nothing -> "??"


viewToolbarButton : String -> Msg -> Bool -> Model -> Html Msg
viewToolbarButton label msg requireSelection model =
  let
    hasNoSelection = List.isEmpty model.selection
    buttonAttr = case requireSelection of
      True ->
        [ stopPropagationOnMousedown NoOp
        , disabled hasNoSelection
        ]
      False -> []
  in
  button
    ( [ onClick msg ]
      ++ buttonAttr
      ++ buttonStyle
    )
    [ text label ]


viewMonadDisplay : Model -> Html Msg
viewMonadDisplay model =
  let
    displayMode = case getSingleSelection model of
      Just (topicId, mapId) -> getDisplayMode topicId mapId model.maps
      Nothing -> Nothing
    (checked1, checked2, disabled_) =
      case displayMode of
        Just (Monad LabelOnly) -> (True, False, False)
        Just (Monad Detail) -> (False, True, False)
        _ -> (False, False, True)
  in
  div
    (displayModeStyle disabled_)
    [ div
        []
        [ text "Monad Display" ]
    , viewRadioButton "Label Only" (SwitchDisplay <| Monad LabelOnly) checked1 disabled_
    , viewRadioButton "Detail" (SwitchDisplay <| Monad Detail) checked2 disabled_
    ]


viewContainerDisplay : Model -> Html Msg
viewContainerDisplay model =
  let
    displayMode = case getSingleSelection model of
      Just (topicId, mapId) -> getDisplayMode topicId mapId model.maps
      Nothing -> Nothing
    (checked1, checked2, checked3) =
      case displayMode of
        Just (Container BlackBox) -> (True, False, False)
        Just (Container WhiteBox) -> (False, True, False)
        Just (Container Unboxed) -> (False, False, True)
        _ -> (False, False, False)
    disabled_ =
      case displayMode of
        Just (Container _) -> False
        _ -> True
  in
  div
    (displayModeStyle disabled_)
    [ div
        []
        [ text "Container Display" ]
    , viewRadioButton "Black Box" (SwitchDisplay <| Container BlackBox) checked1 disabled_
    , viewRadioButton "White Box" (SwitchDisplay <| Container WhiteBox) checked2 disabled_
    , viewRadioButton "Unboxed" (SwitchDisplay <| Container Unboxed) checked3 disabled_
    ]


viewRadioButton : String -> Msg -> Bool -> Bool -> Html Msg
viewRadioButton label_ msg isChecked isDisabled =
  label
    [ stopPropagationOnMousedown NoOp ]
    [ input
      [ type_ "radio", name "display-mode", checked isChecked, disabled isDisabled
      , onClick msg
      ]
      []
    , text label_
    ]


viewFooter : Html Msg
viewFooter =
  div
    footerStyle
    [ div
      []
      [ text version ]
    , div
      []
      [ text date ]
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
  [ style "font-size" <| fromInt footerFontSize ++ "px"
  , style "color" "lightgray"
  ]


linkStyle : List (Attribute Msg)
linkStyle =
  [ style "color" "lightgray" ]


-- UPDATE


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  let
    _ =
      case msg of
        Mouse _ -> msg
        _ -> info "update" msg
  in
  case msg of
    AddTopic -> createTopicAndAddToMap topicDefaultText Nothing (activeMap model) model
      |> storeModel
    MoveTopicToMap topicId mapId origPos targetId targetMapId pos
      -> moveTopicToMap topicId mapId origPos targetId targetMapId pos model |> storeModel
    SwitchDisplay displayMode -> switchDisplay displayMode model |> storeModel
    Search searchMsg -> updateSearch searchMsg model
    Edit editMsg -> updateEdit editMsg model
    IconMenu iconMenuMsg -> updateIconMenu iconMenuMsg model
    Mouse mouseMsg -> updateMouse mouseMsg model
    Nav navMsg -> updateNav navMsg model |> storeModel
    Hide -> hide model |> storeModel
    Delete -> delete model |> storeModel
    NoOp -> (model, Cmd.none)


moveTopicToMap : Id -> MapId -> Point -> Id -> MapId -> Point -> Model -> Model
moveTopicToMap topicId mapId origPos targetId targetMapId pos model =
  let
    (newModel, created) = createMapIfNeeded targetId targetMapId model
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
      |> select targetId targetMapId
      |> autoSize
    Nothing -> model


createMapIfNeeded : Id -> MapId -> Model -> (Model, Bool)
createMapIfNeeded topicId mapId model =
  if hasMap topicId model.maps then
    (model, False)
  else
    ( { model | maps = model.maps |> Dict.insert
        topicId
        (Map topicId Dict.empty (Rectangle 0 0 0 0) mapId)
      }
      |> setDisplayMode topicId mapId (Container BlackBox)
    , True
    )


switchDisplay : DisplayMode -> Model -> Model
switchDisplay displayMode model =
  ( case getSingleSelection model of
    Just (containerId, targetMapId) ->
      { model | maps =
        case displayMode of
          Monad _ -> model.maps
          Container BlackBox -> boxContainer containerId targetMapId model
          Container WhiteBox -> boxContainer containerId targetMapId model
          Container Unboxed -> unboxContainer containerId targetMapId model
      }
      |> setDisplayMode containerId targetMapId displayMode
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
      Just (topicId, mapId) ->
        { model | editState = ItemEdit topicId mapId }
        |> setDetailDisplayIfMonade topicId mapId
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
    Just (topicId, mapId) ->
      { model
      | mapPath = topicId :: model.mapPath
      , selection = []
      }
      |> createMapIfNeeded topicId mapId
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
  , selection = selection
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
        (\(itemId, mapId) modelAcc -> hideItem itemId mapId modelAcc)
        model
  in
  { newModel | selection = [] }
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
  { newModel | selection = [] }
  |> autoSize



-- STYLE


displayModeStyle : Bool -> List (Attribute Msg)
displayModeStyle disabled =
  let
    (color, pointerEvents) =
      if disabled then
        ("gray", "none")
      else
        ("unset", "unset")
  in
  [ style "display" "flex"
  , style "flex-direction" "column"
  , style "gap" "6px"
  , style "color" color
  , style "pointer-events" pointerEvents
  ]


buttonStyle : List (Attribute Msg)
buttonStyle =
  [ style "font-family" mainFont
  , style "font-size" <| fromInt toolbarFontSize ++ "px"
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
