module Main exposing (..)

import Boxing exposing (boxContainer, unboxContainer)
import Config exposing (..)
import IconMenu exposing (viewIconMenu, updateIconMenu)
import MapAutoSize exposing (autoSize)
import MapRenderer exposing (viewMap)
import Model exposing (..)
import Utils exposing (..)

import Browser
import Browser.Dom as Dom
import Browser.Events as Events
import Dict
import Html exposing (Html, Attribute, div, text, br, button, input, label, h1)
import Html.Attributes exposing (id, style, type_, name, checked, disabled)
import Html.Events exposing (onClick, on)
import Random
import String exposing (fromInt, fromFloat)
import Task
import Time exposing (posixToMillis)
import Json.Decode as D
import Debug exposing (toString)



-- MAIN


main =
  Browser.document
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions
    }


init : () -> ( Model, Cmd Msg )
init flags =
  ( { items = Dict.empty
    , maps = Dict.singleton 0
      <| Map 0 Dict.empty (Rectangle 0 0 0 0) -1 -- parentMapId = -1
    , activeMap = 0
    , selection = []
    , editState = NoEdit
    , dragState = NoDrag
    , iconMenuState = False
    , measureText = ""
    , nextId = 1
    }
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
      ( [ h1 [] [ text "DM6 Elm" ]
        , viewToolbar model
        , viewMap model.activeMap -1 model -- parentMapId = -1
        ]
        ++
          if model.iconMenuState then
            [ viewIconMenu model ]
          else
            []
      )
    , div
      ( [ id "measure" ]
        ++ measureStyle
      )
      [ text model.measureText
      , br [] []
      ]
    ]


viewToolbar : Model -> Html Msg
viewToolbar model =
  let
    hasNoSelection = List.isEmpty model.selection
  in
  div
    toolbarStyle
    [ button
        ( [ onClick AddTopic ]
          ++ buttonStyle
        )
        [ text "Add Topic" ]
    , viewMonadMode model
    , viewContainerMode model
    , button
        ( [ onClick (IconMenu Open)
          , stopPropagationOnMousedown NoOp
          , disabled hasNoSelection
          ]
          ++ buttonStyle
        )
        [ text "Choose Icon" ]
    , button
        ( [ onClick (Edit ItemEditStart)
          , stopPropagationOnMousedown NoOp
          , disabled hasNoSelection
          ]
          ++ buttonStyle
        )
        [ text "Edit" ]
    , button
        ( [ onClick Delete
          , stopPropagationOnMousedown NoOp
          , disabled hasNoSelection
          ]
          ++ buttonStyle
        )
        [ text "Delete" ]
    ]


viewMonadMode : Model -> Html Msg
viewMonadMode model =
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
    , label
        [ onClick (Set <| Monad LabelOnly), stopPropagationOnMousedown NoOp ]
        [ input
            [ type_ "radio", name "display-mode", checked checked1, disabled disabled_ ]
            []
        , text "Label Only"
        ]
    , label
        [ onClick (Set <| Monad Detail), stopPropagationOnMousedown NoOp ]
        [ input
            [ type_ "radio", name "display-mode", checked checked2, disabled disabled_ ]
            []
        , text "Detail"
        ]
    ]


viewContainerMode : Model -> Html Msg
viewContainerMode model =
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
    , label
        [ onClick (Set <| Container BlackBox), stopPropagationOnMousedown NoOp ]
        [ input
            [ type_ "radio", name "display-mode", checked checked1, disabled disabled_ ]
            []
        , text "Black Box"
        ]
    , label
        [ onClick (Set <| Container WhiteBox), stopPropagationOnMousedown NoOp ]
        [ input
            [ type_ "radio", name "display-mode", checked checked2, disabled disabled_ ]
            []
        , text "White Box"
        ]
    , label
        [ onClick (Set <| Container Unboxed), stopPropagationOnMousedown NoOp ]
        [ input
            [ type_ "radio", name "display-mode", checked checked3, disabled disabled_ ]
            []
        , text "Unboxed"
        ]
    ]



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
    AddTopic -> (createTopicAndAddToMap model.activeMap model, Cmd.none)
    MoveTopicToMap topicId mapId origPos targetId targetMapId pos
      -> (moveTopicToMap topicId mapId origPos targetId targetMapId pos model, Cmd.none)
    Set displayMode -> (switchDisplayMode displayMode model, Cmd.none)
    Edit editMsg -> updateEdit editMsg model
    IconMenu iconMenuMsg -> (updateIconMenu iconMenuMsg model, Cmd.none)
    Mouse mouseMsg -> updateMouse mouseMsg model
    Delete -> (delete model, Cmd.none)
    NoOp -> (model, Cmd.none)


createTopic : Model -> (Model, Id)
createTopic model =
  let
    id = model.nextId
    topic = TopicInfo id topicDefaultText Nothing
  in
  ( { model
    | items = model.items
      |> Dict.insert id (Topic topic)
    }
    |> nextId
  , id
  )


createTopicAndAddToMap : MapId -> Model -> Model
createTopicAndAddToMap mapId model =
  let
    (newModel, topicId) = createTopic model
    props = ViewTopic <| TopicProps
      (Point 189 97)
      topicDetailSize
      (Monad LabelOnly)
  in
  newModel
  |> addItemToMap topicId props mapId
  |> select topicId mapId


-- Presumption: both players exist in same map
createDefaultAssoc : Id -> Id -> MapId -> Model -> Model
createDefaultAssoc player1 player2 mapId model =
  createAssocAndAddToMap
    "dmx.association"
    player1 "dmx.default"
    player2 "dmx.default"
    mapId model


-- Presumption: both players exist in same map
createAssoc : ItemType -> Id -> RoleType -> Id -> RoleType -> Model -> (Model, Id)
createAssoc itemType player1 role1 player2 role2 model =
  let
    id = model.nextId
    assoc = AssocInfo id itemType player1 role1 player2 role2
  in
  ( { model | items = model.items |> Dict.insert id (Assoc assoc) } |> nextId
  , id
  )


-- Presumption: both players exist in same map
createAssocAndAddToMap : ItemType -> Id -> RoleType -> Id -> RoleType -> MapId -> Model -> Model
createAssocAndAddToMap itemType player1 role1 player2 role2 mapId model =
  let
    (newModel, assocId) = createAssoc itemType player1 role1 player2 role2 model
    props = ViewAssoc AssocProps
  in
  addItemToMap assocId props mapId newModel


nextId : Model -> Model
nextId model =
  { model | nextId = model.nextId + 1 }


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
      addItemToMap topicId viewProps targetId
        { newModel | maps =
            hideItem topicId mapId newModel.maps model
            |> setTopicPos topicId mapId origPos
        }
        |> select targetId targetMapId
        |> updateGeometry
    Nothing -> model


createMapIfNeeded : Id -> MapId -> Model -> (Model, Bool)
createMapIfNeeded targetId targetMapId model =
  if hasMap targetId model.maps then
    (model, False)
  else
    ( { model
      | maps = setDisplayMode targetId targetMapId (Container BlackBox)
          { model
          | maps = model.maps |>
            Dict.insert
              targetId
              (Map targetId Dict.empty (Rectangle 0 0 0 0) targetMapId)
          }
      }
    , True
    )


addItemToMap : Id -> ViewProps -> MapId -> Model -> Model
addItemToMap itemId props mapId model =
  let
    (newModel, parentAssocId) = createAssoc
      "dmx.composition"
      itemId "dmx.child"
      mapId "dmx.parent"
      model
    viewItem = ViewItem itemId False props parentAssocId -- hidden=False
    _ = info "addItemToMap"
      { itemId = itemId, props = props, mapId = mapId, parentAssocId = parentAssocId}
  in
  { newModel | maps =
    updateMaps
      mapId
      (\map -> { map | items = map.items |> Dict.insert itemId viewItem })
      newModel.maps
  }


updateGeometry : Model -> Model
updateGeometry model =
  autoSize model


switchDisplayMode : DisplayMode -> Model -> Model
switchDisplayMode displayMode model =
  let
    maps =
      case getSingleSelection model of
        Just (containerId, targetMapId) ->
          let
            newModel =
              { model | maps =
                case displayMode of
                  Monad _ -> model.maps
                  Container BlackBox -> boxContainer containerId targetMapId model
                  Container WhiteBox -> boxContainer containerId targetMapId model
                  Container Unboxed -> unboxContainer containerId targetMapId model
              }
          in
          setDisplayMode containerId targetMapId displayMode newModel
        Nothing -> model.maps
  in
  { model | maps = maps }
  |> updateGeometry


-- Text Edit

updateEdit : EditMsg -> Model -> (Model, Cmd Msg)
updateEdit msg model =
  case msg of
    ItemEditStart -> startItemEdit model
    ItemEditInput text -> (updateItemText text model, Cmd.none)
    TextareaInput text -> updateTextareaText text model
    SetTopicSize topicId mapId size ->
      ( { model | maps = setTopicSize topicId mapId size model.maps }
        |> updateGeometry
      , Cmd.none
      )
    ItemEditEnd -> (endItemEdit model, Cmd.none)


startItemEdit : Model -> (Model, Cmd Msg)
startItemEdit model =
  let
    newModel = case getSingleSelection model of
      Just (topicId, mapId) ->
        { model | editState = ItemEdit topicId mapId }
        |> setDetailDisplayIfMonade topicId mapId
        |> updateGeometry
      Nothing -> model
  in
  (newModel, focus newModel)


setDetailDisplayIfMonade : Id -> MapId -> Model -> Model
setDetailDisplayIfMonade topicId mapId model =
  { model
  | maps = updateTopicProps topicId mapId model.maps
    (\props ->
      case props.displayMode of
        Monad _ -> { props | displayMode = Monad Detail }
        _ -> props
    )
  }


updateItemText : String -> Model -> Model
updateItemText text model =
  case model.editState of
    ItemEdit topicId _ ->
      updateTopicInfo topicId
        (\topic -> { topic | text = text })
        model
    NoEdit -> logError "updateItemText" "called when editState is NoEdit" model


updateTextareaText : String -> Model -> (Model, Cmd Msg)
updateTextareaText text model =
  case model.editState of
    ItemEdit topicId mapId ->
      updateTopicInfo topicId
        (\topic -> { topic | text = text })
        model
      |> measureText text topicId mapId
    NoEdit -> logError "updateTextareaText" "called when editState is NoEdit" (model, Cmd.none)


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


endItemEdit : Model -> Model
endItemEdit model =
  { model | editState = NoEdit }
  |> updateGeometry


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


-- Mouse

updateMouse : MouseMsg -> Model -> ( Model, Cmd Msg )
updateMouse msg model =
  case msg of
    Down -> ( mouseDown model, Cmd.none )
    DownItem class id mapId pos -> mouseDownOnItem model class id mapId pos
    Move pos -> mouseMove model pos
    Up -> mouseUp model
    Over class id mapId -> ( mouseOver model class id mapId, Cmd.none )
    Out class id mapId -> ( mouseOut model class id mapId, Cmd.none )
    Time time -> ( timeArrived time model, Cmd.none )


mouseDown : Model -> Model
mouseDown model =
  { model | selection = [] }


mouseDownOnItem : Model -> Class -> Id -> MapId -> Point -> ( Model, Cmd Msg )
mouseDownOnItem model class id mapId pos =
  ( { model | dragState = WaitForStartTime class id mapId pos
    } |> select id mapId
  , Task.perform (Time >> Mouse) Time.now
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


mouseMove : Model -> Point -> ( Model, Cmd Msg )
mouseMove model pos =
  case model.dragState of
    DragEngaged time class id mapId pos_ ->
      ( { model | dragState = WaitForEndTime time class id mapId pos_ }
      , Task.perform (Time >> Mouse) Time.now
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
        maps =
          case dragMode of
            DragTopic -> setTopicPosByDelta id mapId delta model.maps
            DrawAssoc -> model.maps
      in
      { model
        | maps = maps
        , dragState = Drag dragMode id mapId origPos pos target -- update lastPos
      } |> updateGeometry
    _ -> logError "performDrag"
      ("Received \"Move\" message when dragState is " ++ toString model.dragState)
      model


mouseUp : Model -> ( Model, Cmd Msg )
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


strToIntDecoder : String -> D.Decoder Int
strToIntDecoder str =
  case String.toInt str of
    Just int -> D.succeed int
    Nothing -> D.fail <| "\"" ++ str ++ "\" is an invalid ID"


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


appStyle : List (Attribute Msg)
appStyle =
  [ style "font-family" mainFont
  , style "font-size" <| fromInt mainFontSize ++ "px"
  , style "user-select" "none"
  , style "-webkit-user-select" "none" -- Safari still needs vendor prefix
  ]


toolbarStyle : List (Attribute Msg)
toolbarStyle =
  [ style "display" "inline-flex"
  , style "flex-direction" "column"
  , style "align-items" "flex-start"
  , style "gap" "28px"
  , style "margin-top" "20px"
  , style "position" "relative"
  , style "z-index" "3"
  ]


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
  , style "font-size" <| fromInt mainFontSize ++ "px"
  ]


measureStyle : List (Attribute Msg)
measureStyle =
  [ style "position" "fixed"
  , style "visibility" "hidden"
  , style "white-space" "pre-wrap"
  , style "font-family" mainFont
  , style "font-size" <| fromInt mainFontSize ++ "px"
  , style "line-height" <| fromFloat topicLineHeight
  , style "padding" <| fromInt topicDetailPadding ++ "px"
  , style "width" <| fromFloat topicDetailMaxWidth ++ "px"
  , style "min-width" <| fromFloat (topicSize.w - topicSize.h) ++ "px"
  , style "max-width" "max-content"
  , style "border-width" <| fromFloat topicBorderWidth ++ "px"
  , style "border-style" "solid"
  , style "box-sizing" "border-box"
  ]
