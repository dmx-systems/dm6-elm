module Main exposing (..)

import Model exposing (..)
import Config exposing (..)
import MapRenderer exposing (viewMap)
import MapAutoSize exposing (autoSize)
import Boxing exposing (boxContainer, unboxContainer)
import IconMenu exposing (updateIconMenu, viewIconMenu)

import Array
import Browser
import Browser.Dom as Dom
import Browser.Events as Events
import Dict exposing (Dict)
import Html exposing (Html, Attribute, div, text, button, input, label, h1)
import Html.Attributes exposing
  (class, id, style, attribute, type_, name, value, checked, disabled)
import Html.Events exposing (onClick, on)
import Random
import String exposing (String, fromInt)
import Task
import Time exposing (posixToMillis)
import Json.Decode as D
import Debug exposing (log, toString)



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
          , stopPropagationOnMousedown
          , disabled hasNoSelection
          ]
          ++ buttonStyle
        )
        [ text "Choose Icon" ]
    , button
        ( [ onClick (Edit ItemEditStart)
          , stopPropagationOnMousedown
          , disabled hasNoSelection
          ]
          ++ buttonStyle
        )
        [ text "Edit Text" ]
    , button
        ( [ onClick Delete
          , stopPropagationOnMousedown
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
        [ onClick (Set <| Monad LabelOnly), stopPropagationOnMousedown ]
        [ input
            [ type_ "radio", name "display-mode", checked checked1, disabled disabled_ ]
            []
        , text "Label Only"
        ]
    , label
        [ onClick (Set <| Monad Detail), stopPropagationOnMousedown ]
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
        [ onClick (Set <| Container BlackBox), stopPropagationOnMousedown ]
        [ input
            [ type_ "radio", name "display-mode", checked checked1, disabled disabled_ ]
            []
        , text "Black Box"
        ]
    , label
        [ onClick (Set <| Container WhiteBox), stopPropagationOnMousedown ]
        [ input
            [ type_ "radio", name "display-mode", checked checked2, disabled disabled_ ]
            []
        , text "White Box"
        ]
    , label
        [ onClick (Set <| Container Unboxed), stopPropagationOnMousedown ]
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
    AddTopic -> (createTopicAndAddToMap model, Cmd.none)
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
    text = "New Topic"
    topic = TopicInfo id text Nothing
  in
  ( { model
    | items = model.items
      |> Dict.insert id (Topic topic)
    }
    |> nextId
  , id
  )


createTopicAndAddToMap : Model -> Model
createTopicAndAddToMap model =
  let
    (newModel, topicId) = createTopic model
    props = ViewTopic <| TopicProps pos (Monad LabelOnly)
    pos = Point 175 98
  in
  newModel
  |> addItemToMap topicId props model.activeMap
  |> select topicId model.activeMap


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
          (topicSize.w / 2 + whiteBoxPadding)
          (topicSize.h / 2 + whiteBoxPadding)
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
  { model | maps = autoSize model.activeMap model.maps }


-- Display Mode

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
                  Monad _ -> model.maps -- TODO: Monad Detail
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
    ItemEditEnd -> (endItemEdit model, Cmd.none)


startItemEdit : Model -> (Model, Cmd Msg)
startItemEdit model =
  let
    newModel = case getSingleSelection model of
      Just (topicId, mapId) ->
        { model | editState = ItemEdit topicId }
        |> setDetailDisplayIfMonade topicId mapId
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
    ItemEdit id -> updateTopicInfo id
      (\topic -> { topic | text = text })
      model
    NoEdit -> logError "updateItemText" "called when editState is NoEdit" model


endItemEdit : Model -> Model
endItemEdit model =
  { model | editState = NoEdit }


focus : Model -> Cmd Msg
focus model =
  let
    nodeId =
      case model.editState of
        ItemEdit id -> "dmx-input-" ++ String.fromInt id
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
    maps = updateMaps
      model.activeMap -- FIXME: delete items from other/different maps
      (deleteViewItems model.selection)
      model.maps
  in
  { model
  -- TODO: iterate model.selection only once?
  | items = deleteItems model.selection model.items
  , maps = maps
  , selection = []
  }


deleteItems : Selection -> Items -> Items
deleteItems selItems items =
  case selItems of
    [] -> items
    (id, mapId) :: moreSelItems -> deleteItems moreSelItems (Dict.remove id items)
    -- FIXME: delete assocs where the item is a player


deleteViewItems : Selection -> Map -> Map
deleteViewItems selItems map =
  case selItems of
    [] -> map
    (id, mapId) :: moreSelItems -> deleteViewItems moreSelItems (removeItemFromMap_ id map)
    -- FIXME: delete assocs where the item is a player


removeItemFromMap : Id -> MapId -> Maps -> Maps
removeItemFromMap itemId mapId maps =
  updateMaps mapId (removeItemFromMap_ itemId) maps


removeItemFromMap_ : Id -> Map -> Map
removeItemFromMap_ itemId map =
  { map | items = map.items |> Dict.remove itemId }


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
  { model
  | selection = []
  , editState = NoEdit
  }


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
              delay = posixToMillis time - posixToMillis startTime > dragThresholdMillis
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
    cx = topicSize.w / 2 + whiteBoxPadding
    cy = topicSize.h / 2 + whiteBoxPadding
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



-- HELPER


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


topicCount : Model -> Int
topicCount model =
  model.items |> Dict.values |> List.filter topicFilter |> List.length


topicFilter : Item -> Bool
topicFilter item =
  case item of
    Topic _ -> True
    Assoc _ -> False



-- STYLE


appStyle : List (Attribute Msg)
appStyle =
  [ style "font-family" "sans-serif"
  , style "font-size" mainFontSize
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
  [ style "font-family" "sans-serif"
  , style "font-size" mainFontSize
  ]
