module Feature.Search exposing (viewInput, viewSearchResult, viewTraversalResult, traverse,
  closeMenu, update)

import Box
import Config as C
import ExtensionDef exposing (AutoSize)
import Feature.Icon as Icon
import Feature.Nav as Nav
import Feature.SearchDef as SearchDef exposing (SearchResult(..))
import Feature.Sel as Sel
import Item
import Model exposing (Model, Msg(..))
import ModelBase exposing (..)
import Size
import Storage as S
import TopicMap.TopicMap as TM
import TopicMap.TopicMapDef exposing (ItemProps(..))
import Undo exposing (UndoModel)
import Utils as U

import Dict
import Html exposing (Html, button, div, input, text)
import Html.Attributes exposing (class, disabled, style, title, value)
import Html.Events exposing (on, onClick, onInput, onFocus)
import Json.Decode as D
import String exposing (fromInt)



-- VIEW


viewInput : Model -> Html Msg
viewInput model =
  let
    target = TM.landingTarget model
  in
  div
    []
    [ input
      ( [ value model.search.term
        , onInput (Search << SearchDef.Input)
        , onFocus (Search SearchDef.InputFocused)
        , U.onMouseDownStop <| Cancel target -- Don't clear selection
        ]
        ++ searchInputStyle
      )
      []
    ]


searchInputStyle : Attrs Msg
searchInputStyle =
  [ style "width" "100px"
  , style "font-family" C.mainFont
  , style "font-size" <| fromInt C.contentFontSize ++ "px"
  ]


viewSearchResult : Model -> List (Html Msg)
viewSearchResult model =
  case model.search.result of
    Topics topicIds _ ->
      if not (topicIds |> List.isEmpty) then
        [ viewSearchtMenu topicIds model ]
      else
        []
    _ -> []


viewTraversalResult : Model -> List (Html Msg)
viewTraversalResult model =
  case model.search.result of
    RelTopics relTopicIds _ ->
      if not (relTopicIds |> List.isEmpty) then
        [ viewTraversalMenu relTopicIds model ]
      else
        []
    _ -> []


viewSearchtMenu : List Id -> Model -> Html Msg
viewSearchtMenu topicIds model =
  div
    ( [ U.onMouseDownStop NoOp ] -- Prevent search menu closing
      ++ searchResultStyle
      ++ menuStyle
    )
    (topicIds |> List.map
      (\id ->
        let
          isDisabled = isItemDisabled id model
          isHover = isTopicHover id model
        in
        case Item.topicById id model of
          Just topic ->
            div
              ( [ onClick <| Search <| SearchDef.TopicClicked id
                , on "pointerover" <| D.succeed <| Search <| SearchDef.TopicHovered id
                , on "pointerout" <| D.succeed <| Search <| SearchDef.TopicUnhovered id
                ]
                ++ menuItemStyle isDisabled isHover
              )
              [ viewTopicIcon topic model
              , viewItemText topic
              , viewFullscreenButton id model
              ]
          Nothing -> text "??"
      )
    )


viewTraversalMenu : List (Id, Id) -> Model -> Html Msg
viewTraversalMenu relTopicIds model =
  div
    ( [ U.onMouseDownStop NoOp ] -- Prevent traversal menu closing
      ++ traversalResultStyle
      ++ menuStyle
    )
    (relTopicIds |> List.map
      (\((id, assocId) as relTopic) ->
        let
          isDisabled = isItemDisabled id model
          isHover = isRelTopicHover relTopic model
        in
        case Item.topicById id model of
          Just topic ->
            div
              ( [ onClick <| Search <| SearchDef.RelTopicClicked relTopic
                , on "pointerover" <| D.succeed <| Search <| SearchDef.RelTopicHovered relTopic
                , on "pointerout" <| D.succeed <| Search <| SearchDef.RelTopicUnhovered relTopic
                ]
                ++ menuItemStyle isDisabled isHover
              )
              [ viewTopicIcon topic model
              , viewItemText topic -- TODO: render assoc info
              , viewFullscreenButton id model
              ]
          Nothing -> text "??"
      )
    )


searchResultStyle : Attrs Msg
searchResultStyle =
  [ style "top" <| fromInt (C.appHeaderHeight - 5) ++ "px"
  , style "right" "48px"
  , style "z-index" "5"
  ]


traversalResultStyle : Attrs Msg
traversalResultStyle =
  [ style "left" "65px" ]


menuStyle : Attrs Msg
menuStyle =
  [ style "position" "absolute"
  , style "width" "210px"
  , style "padding" "3px 0"
  , style "font-size" <| fromInt C.contentFontSize ++ "px"
  , style "line-height" "2"
  , style "background-color" "white"
  , style "border" "1px solid lightgray"
  ]


menuItemStyle : Bool -> Bool -> Attrs Msg
menuItemStyle isDisabled isHover =
  let
    (color, bgColor, pointerEvents) =
      case isDisabled of
        True ->
          (C.disabledColor, "unset", "none")
        False ->
          ( "unset"
          , if isHover then C.hoverColor else "unset"
          , "unset"
          )
  in
  [ style "display" "flex"
  , style "align-items" "center"
  , style "gap" "8px"
  , style "color" color
  , style "background-color" bgColor
  , style "padding" "0 8px"
  , style "pointer-events" pointerEvents
  ]


viewTopicIcon : TopicInfo -> Model -> Html Msg
viewTopicIcon topic model =
  Icon.viewTopicIcon topic.id C.topicIconSize [ style "flex" "none" ] model


viewItemText : TopicInfo -> Html Msg
viewItemText topic =
  div
    [ style "flex" "auto"
    , style "overflow" "hidden"
    , style "text-overflow" "ellipsis"
    , style "white-space" "nowrap"
    ]
    [ text <| Item.topicLabel topic ]


viewFullscreenButton : Id -> Model -> Html Msg
viewFullscreenButton id model =
  let
    isDisabled = model.boxId == id
  in
  case Item.isBox id model of
    True ->
      button
        ( [ class "tool"
          , title "Fullscreen"
          , disabled isDisabled
          , U.onClickStop <| Search <| SearchDef.Fullscreen id -- don't trigger menu item
          , U.onMouseOverStop NoOp -- don't highlight menu item along with button
          , U.onMouseOutStop NoOp -- don't highlight menu item along with button
          ]
          ++ fullscreenButtonStyle
        )
        [ Icon.view "maximize-2" 16 [] ]
    False -> text ""


fullscreenButtonStyle : Attrs Msg
fullscreenButtonStyle =
  [ style "border" "none"
  , style "background-color" "transparent"
  , style "pointer-events" "initial"
  ]


isItemDisabled : Id -> Model -> Bool
isItemDisabled topicId model =
  case TM.revelationBoxId model of
    Just boxId -> Box.hasDeepItem topicId boxId model
    Nothing -> False


isTopicHover : Id -> Model -> Bool
isTopicHover topicId model =
  case model.search.result of
    Topics _ (Just topicId_) -> topicId_ == topicId
    _ -> False


isRelTopicHover : (Id, Id) -> Model -> Bool
isRelTopicHover relTopic model =
  case model.search.result of
    RelTopics _ (Just relTopic_) -> relTopic_ == relTopic
    _ -> False



-- UPDATE


update : SearchDef.Msg -> AutoSize -> UndoModel -> (UndoModel, Cmd Msg)
update msg autoSize ({present} as undoModel) =
  case msg of
    -- Search
    SearchDef.Input term -> (setSearchTerm term present, Cmd.none) |> Undo.swap undoModel
    SearchDef.InputFocused -> (onInputFocused present, Cmd.none) |> Undo.swap undoModel
    SearchDef.TopicHovered topicId -> (onTopicHovered topicId autoSize present, Cmd.none)
      |> Undo.swap undoModel
    SearchDef.TopicUnhovered _ -> (onTopicUnhovered autoSize present, Cmd.none)
      |> Undo.swap undoModel
    SearchDef.TopicClicked topicId -> revealTopic topicId autoSize present |> S.store
      |> Undo.push undoModel
    -- Traverse
    SearchDef.RelTopicHovered relTopicId ->
      (onRelTopicHovered relTopicId autoSize present, Cmd.none) |> Undo.swap undoModel
    SearchDef.RelTopicUnhovered _ -> (onRelTopicUnhovered autoSize present, Cmd.none)
      |> Undo.swap undoModel
    SearchDef.RelTopicClicked relTopicId -> revealRelTopic relTopicId autoSize present
      |> S.store |> Undo.push undoModel
    -- Fullscreen (Search & Traverse)
    SearchDef.Fullscreen boxId -> (undoModel, Nav.pushUrl boxId)


setSearchTerm : String -> Model -> Model
setSearchTerm term model =
  model
    |> setTerm term
    |> searchTopics


onInputFocused : Model -> Model
onInputFocused =
  searchTopics


onTopicHovered : Id -> AutoSize -> Model -> Model
onTopicHovered topicId autoSize model =
  case model.search.result of
    Topics topicIds _ ->
      -- update hover state
      model
        |> setResult (Topics topicIds <| Just topicId)
        |> Size.auto autoSize
    _ ->
      U.logError "onTopicHovered" "search.result is not Topics" model


onRelTopicHovered : (Id, Id) -> AutoSize -> Model -> Model
onRelTopicHovered relTopicId autoSize model =
  case model.search.result of
    RelTopics relTopicIds _ ->
      -- update hover state
      model
        |> setResult (RelTopics relTopicIds <| Just relTopicId)
        |> Size.auto autoSize
    _ ->
      U.logError "onRelTopicHovered" "search.result is not RelTopics" model


onTopicUnhovered : AutoSize -> Model -> Model
onTopicUnhovered autoSize model =
  case model.search.result of
    Topics topicIds _ ->
      -- update hover state
      model
        |> setResult (Topics topicIds Nothing)
        |> Size.auto autoSize
    _ ->
      U.logError "onTopicUnhovered" "search.result is not Topics" model


onRelTopicUnhovered : AutoSize -> Model -> Model
onRelTopicUnhovered autoSize model =
  case model.search.result of
    RelTopics relTopicIds _ ->
      -- update hover state
      model
        |> setResult (RelTopics relTopicIds Nothing)
        |> Size.auto autoSize
    _ ->
      U.logError "onRelTopicUnhovered" "search.result is not RelTopics" model


revealTopic : Id -> AutoSize -> Model -> Model
revealTopic topicId autoSize model =
  case TM.revelationBoxPath model of
    Just (boxId :: _ as boxPath) ->
      model
        |> revealItem topicId boxId
        |> closeMenu
        |> Sel.select topicId boxPath
        |> Size.auto autoSize
    _ -> model


revealRelTopic : (Id, Id) -> AutoSize -> Model -> Model
revealRelTopic (topicId, assocId) autoSize model =
  case TM.revelationBoxPath model of
    Just (boxId :: _ as boxPath) ->
      model
        |> revealItem topicId boxId
        |> revealItem assocId boxId
        |> closeMenu
        |> Sel.select topicId boxPath
        |> Size.auto autoSize
    _ -> model


revealItem : Id -> BoxId -> Model -> Model
revealItem itemId boxId model =
  if Box.hasItem boxId itemId model then
    let
      _ = U.info "revealItem" <| fromInt itemId ++ " is in " ++ fromInt boxId
    in
    TM.showItem itemId boxId model
  else
    let
      _ = U.info "revealItem" <| fromInt itemId ++ " not in " ++ fromInt boxId
      props = TM.initItemProps itemId boxId model
    in
    model
      |> Box.addItem (ItemProps itemId (displayModeFrom props)) boxId
      |> TM.addItem itemId props boxId


-- ### TODO: model Box ItemProps for topic/assoc
displayModeFrom : ItemProps -> DisplayMode
displayModeFrom props =
  case props of
    TopicP {displayMode} -> displayMode
    AssocP _ -> TopicD LabelOnly  -- ### FIXME


-- "searchTopics" instead "search" avoids shadowing
searchTopics : Model -> Model
searchTopics model =
  let
    topicIds = model.items |> Dict.foldr
      (\id item topicIdsAcc ->
        case item.info of
          Topic {text} ->
            case isMatch text model.search.term of
              True -> id :: topicIdsAcc
              False -> topicIdsAcc
          Assoc _ -> topicIdsAcc
      )
      []
  in
  model |> setResult (Topics topicIds Nothing)


traverse : Model -> Model
traverse model =
  let
    relTopicIds =
      case Sel.single model of
        Just (itemId, _) ->
          Item.relatedItems itemId model
        Nothing -> [] -- TODO: log error
  in
  model |> setResult (RelTopics relTopicIds Nothing)


isMatch : String -> String -> Bool
isMatch text searchTerm =
  not (searchTerm |> String.isEmpty)
    && String.contains (String.toLower searchTerm) (String.toLower text)


closeMenu : Model -> Model
closeMenu model =
  model |> setResult NoSearch


setTerm : String -> Model -> Model
setTerm term ({search} as model) =
  { model | search = { search | term = term }}


setResult : SearchResult -> Model -> Model
setResult result ({search} as model) =
  { model | search = { search | result = result }}
