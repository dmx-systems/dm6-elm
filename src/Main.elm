module Main exposing (..)

import Boxing exposing (boxContainer, unboxContainer)
import Config exposing (..)
import Search exposing (viewSearchInput, viewSearchResult, updateSearch)
import IconMenu exposing (viewIcon, viewIconMenu, updateIconMenu)
import MapAutoSize exposing (autoSize)
import MapRenderer exposing (viewMap)
import Model exposing (..)
import Storage exposing (storeModel, storeModelWith, modelDecoder)
import Utils exposing (..)

import Browser
import Browser.Dom as Dom
import Browser.Events as Events
import Dict
import Html exposing (Html, Attribute, div, span, text, br, button, input, label, a)
import Html.Attributes exposing (id, href, style, type_, name, checked, disabled)
import Html.Events exposing (onClick, on)
import Random
import String exposing (fromInt, fromFloat)
import Task
import Time exposing (posixToMillis)
import Json.Decode as D
import Json.Encode as E
import Debug exposing (log, toString)



-- MAIN


main : Program E.Value Model Msg
main =
  Browser.document
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions
    }


init : E.Value -> (Model, Cmd Msg)
init flags =
  ( case D.decodeValue modelDecoder flags of
    Ok model ->
      log "Reading localStorage" model
    Err e ->
      let
        _ = logError "init" "Could not read localStorage" e
      in
      defaultModel
  , Cmd.none
  )



-- VIEW


view : Model -> Browser.Document Msg
view model =
  Browser.Document
    "DM6 Elm"
    [ div
      ( [ on "mouseover" (mouseDecoder Over)
        , on "mouseout" (mouseDecoder Out)
        ]
        ++ appStyle
      )
      ( [ viewToolbar model
        , viewMap (activeMap model) -1 model -- parentMapId = -1
        ]
        ++ viewSearchResult model
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
    [ onClick msg
    , stopPropagationOnMousedown NoOp
    ]
    [ input
      [ type_ "radio", name "display-mode", checked isChecked, disabled isDisabled ]
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
    AddTopic -> createTopicAndAddToMap (activeMap model) model |> storeModel
    MoveTopicToMap topicId mapId origPos targetId targetMapId pos
      -> moveTopicToMap topicId mapId origPos targetId targetMapId pos model |> storeModel
    SwitchDisplay displayMode -> switchDisplay displayMode model |> storeModel
    Search searchMsg -> updateSearch searchMsg model
    Edit editMsg -> updateEdit editMsg model
    IconMenu iconMenuMsg -> updateIconMenu iconMenuMsg model
    Mouse mouseMsg -> updateMouse mouseMsg model
    Nav navMsg -> updateNav navMsg model |> storeModel
    Delete -> delete model |> storeModel
    NoOp -> (model, Cmd.none)


createTopicAndAddToMap : MapId -> Model -> Model
createTopicAndAddToMap mapId model =
  case getMap mapId model.maps of
    Just map ->
      let
        (newModel, topicId) = createTopic topicDefaultText Nothing model
        props = ViewTopic <| TopicProps
          (Point
            (newTopicPos.x + map.rect.x1)
            (newTopicPos.y + map.rect.y1)
          )
          topicDetailSize
          (Monad LabelOnly)
      in
      newModel
      |> addItemToMap topicId props mapId
      |> select topicId mapId
    Nothing -> model


-- Presumption: both players exist in same map
createDefaultAssoc : Id -> Id -> MapId -> Model -> Model
createDefaultAssoc player1 player2 mapId model =
  createAssocAndAddToMap
    "dmx.association"
    player1 "dmx.default"
    player2 "dmx.default"
    mapId model


-- Presumption: both players exist in same map
createAssocAndAddToMap : ItemType -> Id -> RoleType -> Id -> RoleType -> MapId -> Model -> Model
createAssocAndAddToMap itemType player1 role1 player2 role2 mapId model =
  let
    (newModel, assocId) = createAssoc itemType player1 role1 player2 role2 model
    props = ViewAssoc AssocProps
  in
  addItemToMap assocId props mapId newModel


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
    viewProps_ =
      getTopicProps topicId mapId newModel.maps
      |> Maybe.andThen (\props -> Just (ViewTopic { props | pos = newPos }))
  in
  case viewProps_ of
    Just viewProps ->
      newModel
      |> hideItem topicId mapId
      |> setTopicPos topicId mapId origPos
      |> addItemToMap topicId viewProps targetId
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


delete : Model -> Model
delete model =
  let
    newModel = model.selection
      |> List.map Tuple.first
      |> List.foldr
        (\itemId model_ -> deleteItem itemId model_)
        model
  in
  { newModel | selection = [] }
  |> autoSize


-- Mouse

updateMouse : MouseMsg -> Model -> (Model, Cmd Msg)
updateMouse msg model =
  case msg of
    Down -> ( mouseDown model, Cmd.none )
    DownItem class id mapId pos -> mouseDownOnItem model class id mapId pos
    Move pos -> mouseMove model pos
    Up -> mouseUp model |> storeModelWith
    Over class id mapId -> ( mouseOver model class id mapId, Cmd.none )
    Out class id mapId -> ( mouseOut model class id mapId, Cmd.none )
    Time time -> ( timeArrived time model, Cmd.none )


mouseDown : Model -> Model
mouseDown model =
  { model
  | selection = []
  , iconMenuState = False
  , searchMenu = ResultClosed
  }


mouseDownOnItem : Model -> Class -> Id -> MapId -> Point -> (Model, Cmd Msg)
mouseDownOnItem model class id mapId pos =
  ( { model | dragState = WaitForStartTime class id mapId pos
    } |> select id mapId
  , Task.perform (Mouse << Time) Time.now
  )


timeArrived : Time.Posix -> Model -> Model
timeArrived time model =
  case model.dragState of
    WaitForStartTime class id mapId pos ->
      { model | dragState = DragEngaged time class id mapId pos }
    WaitForEndTime startTime class id mapId pos ->
      { model | dragState =
        case class of
          "dmx-topic" ->
            let
              delay = posixToMillis time - posixToMillis startTime > assocDelayMillis
              dragMode = if delay then DrawAssoc else DragTopic
              origPos_ = getTopicPos id mapId model.maps
            in
            case origPos_ of
              Just origPos -> Drag dragMode id mapId origPos pos Nothing
              Nothing -> NoDrag
          _ -> NoDrag -- the error will be logged in performDrag
      }
    _ -> logError "timeArrived"
      "Received \"Time\" message when dragState is not WaitForTime"
      model


mouseMove : Model -> Point -> (Model, Cmd Msg)
mouseMove model pos =
  case model.dragState of
    DragEngaged time class id mapId pos_ ->
      ( { model | dragState = WaitForEndTime time class id mapId pos_ }
      , Task.perform (Mouse << Time) Time.now
      )
    WaitForEndTime _ _ _ _ _ ->
      ( model, Cmd.none ) -- ignore -- TODO: can this happen at all? Is there a move listener?
    Drag _ _ _ _ _ _ ->
      ( performDrag model pos, Cmd.none )
    _ -> logError "mouseMove"
      ("Received \"Move\" message when dragState is " ++ toString model.dragState)
      ( model, Cmd.none )


performDrag : Model -> Point -> Model
performDrag model pos =
  case model.dragState of
    Drag dragMode id mapId origPos lastPos target ->
      let
        delta = Point
          (pos.x - lastPos.x)
          (pos.y - lastPos.y)
        newModel =
          case dragMode of
            DragTopic -> setTopicPosByDelta id mapId delta model
            DrawAssoc -> model
      in
      { newModel | dragState = Drag dragMode id mapId origPos pos target } -- update lastPos
      |> autoSize
    _ -> logError "performDrag"
      ("Received \"Move\" message when dragState is " ++ toString model.dragState)
      model


mouseUp : Model -> (Model, Cmd Msg)
mouseUp model =
  let
    (newModel, cmd) =
      case model.dragState of
        Drag DragTopic id mapId origPos _ (Just (targetId, targetMapId)) ->
          let
            _ = info "mouseUp" ("dropped " ++ fromInt id ++ " (map " ++ fromInt mapId
              ++ ") on " ++ fromInt targetId ++ " (map " ++ fromInt targetMapId ++ ") --> "
              ++ if notDroppedOnOwnMap then "move topic" else "abort")
            notDroppedOnOwnMap = mapId /= targetId
            msg = MoveTopicToMap id mapId origPos targetId targetMapId
          in
          if notDroppedOnOwnMap then
            (model, Random.generate msg point)
          else
            (model, Cmd.none)
        Drag DrawAssoc id mapId _ _ (Just (targetId, targetMapId)) ->
          let
            _ = info "mouseUp" ("assoc drawn from " ++ fromInt id ++ " (map " ++ fromInt mapId
              ++ ") to " ++ fromInt targetId ++ " (map " ++ fromInt targetMapId ++ ") --> "
              ++ if isSameMap then "create assoc" else "abort")
            isSameMap = mapId == targetMapId
          in
          if isSameMap then
            (createDefaultAssoc id targetId mapId model, Cmd.none)
          else
            (model, Cmd.none)
        Drag _ id mapId _ _ _ ->
          let
            _ = info "mouseUp" "drag ended w/o target"
            {- _ = case getTopicProps id mapId model.maps of
              Just props ->
                log "" { id = id, pos = props.pos }
              Nothing -> { id = 0, pos = Point 0 0 }
            _ = case getMapIfExists id model.maps of
              Just map -> log "" { rect = map.rect }
              Nothing -> { rect = Rectangle 0 0 0 0 } -}
          in
          (model, Cmd.none)
        DragEngaged _ _ _ _ _ ->
          let
            _ = info "mouseUp" "drag aborted w/o moving"
          in
          (model, Cmd.none)
        _ ->
          logError "mouseUp"
            ("Received \"Up\" message when dragState is " ++ toString model.dragState)
            (model, Cmd.none)
  in
  ({ newModel | dragState = NoDrag }, cmd)


point : Random.Generator Point
point =
  let
    cx = topicW2 + whiteBoxPadding
    cy = topicH2 + whiteBoxPadding
    rw = whiteBoxRange.w
    rh = whiteBoxRange.h
  in
  Random.map2
    (\x y -> Point (cx + x) (cy + y))
    (Random.float 0 rw)
    (Random.float 0 rh)


mouseOver : Model -> Class -> Id -> MapId -> Model
mouseOver model class targetId targetMapId =
  case model.dragState of
    Drag dragMode id mapId origPos lastPos _ ->
      let
        target =
          if (id, mapId) /= (targetId, targetMapId) then
            Just (targetId, targetMapId)
          else
            Nothing
      in
      { model | dragState = Drag dragMode id mapId origPos lastPos target } -- update target
    DragEngaged _ _ _ _ _ ->
      logError "mouseOver" "Received \"Over\" message when dragState is DragEngaged" model
    _ -> model


mouseOut : Model -> Class -> Id -> MapId -> Model
mouseOut model class targetId targetMapId =
  case model.dragState of
    Drag dragMode id mapId origPos lastPos _ ->
      { model | dragState = Drag dragMode id mapId origPos lastPos Nothing } -- reset target
    _ -> model



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
  case model.dragState of
    NoDrag -> mouseDownSub
    WaitForStartTime _ _ _ _ -> Sub.none
    WaitForEndTime _ _ _ _ _ -> Sub.none
    DragEngaged _ _ _ _ _ -> dragSub
    Drag _ _ _ _ _ _ -> dragSub


mouseDownSub : Sub Msg
mouseDownSub =
  Events.onMouseDown <| D.oneOf
    [ D.map Mouse <| D.map4 DownItem
        ( D.oneOf
          [ D.at ["target", "className"] D.string -- HTML elements
          , D.at ["target", "className", "baseVal"] D.string -- SVG elements
          ]
        )
        ( D.at ["target", "dataset", "id"] D.string |> D.andThen strToIntDecoder )
        ( D.at ["target", "dataset", "mapId"] D.string |> D.andThen strToIntDecoder )
        ( D.map2 Point -- TODO: no code doubling
          ( D.field "clientX" D.float )
          ( D.field "clientY" D.float )
        )
    , D.succeed (Mouse Down)
    ]


dragSub : Sub Msg
dragSub =
  Sub.batch
    [ Events.onMouseMove <| D.map Mouse <| D.map Move
        ( D.map2 Point -- TODO: no code doubling
          ( D.field "clientX" D.float )
          ( D.field "clientY" D.float )
        )
    , Events.onMouseUp <| D.map Mouse <| D.succeed Up
    ]


-- TODO: no code doubling
mouseDecoder : (Class -> Id -> MapId -> MouseMsg) -> D.Decoder Msg
mouseDecoder msg =
  D.map Mouse <| D.map3 msg
    ( D.oneOf
      [ D.at ["target", "className"] D.string -- HTML elements
      , D.at ["target", "className", "baseVal"] D.string -- SVG elements
      ]
    )
    ( D.at ["target", "dataset", "id"] D.string |> D.andThen strToIntDecoder )
    ( D.at ["target", "dataset", "mapId"] D.string |> D.andThen strToIntDecoder )



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
