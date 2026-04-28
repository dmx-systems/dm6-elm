module Feature.Search exposing (viewInput, viewSearchResult, viewTraversalResult, traverse,
  closeMenu, update)

import Assoc
import Box
import Config as C
import Env exposing (Env)
import Feature.Icon as Icon
import Feature.Nav as Nav
import Feature.SearchDef as SearchDef exposing (SearchResult(..))
import Feature.Sel as Sel
import Model exposing (Model, Msg(..))
import ModelBase exposing (..)
import Storage as S
import Topic
import TopicMap.TopicMap as TM
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
        , U.onPointerDownStop <| Cancel target -- Don't clear selection
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


viewSearchtMenu : List TopicId -> Model -> Html Msg
viewSearchtMenu topicIds model =
  div
    ( [ U.onPointerDownStop NoOp ] -- Prevent search menu closing
      ++ searchResultStyle
      ++ menuStyle
    )
    (topicIds |> List.map
      (\id ->
        let
          isDisabled = isItemDisabled id model
          isHover = isTopicHover id model
        in
        case Topic.fromId id model of
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
              , viewFullscreenButton (toTopicId id) model
              ]
          Nothing -> text "??"
      )
    )


viewTraversalMenu : List (TopicId, AssocId) -> Model -> Html Msg
viewTraversalMenu relTopicIds model =
  div
    ( [ U.onPointerDownStop NoOp ] -- Prevent traversal menu closing
      ++ traversalResultStyle
      ++ menuStyle
    )
    (relTopicIds |> List.map
      (\((id, assocId) as relTopic) ->
        let
          isDisabled = isItemDisabled id model
          isHover = isRelTopicHover relTopic model
        in
        case Topic.fromId id model of
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
              , viewFullscreenButton (toTopicId id) model
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


viewTopicIcon : Topic -> Model -> Html Msg
viewTopicIcon topic model =
  Icon.viewTopicIcon topic.id C.topicIconSize [ style "flex" "none" ] model


viewItemText : Topic -> Html Msg
viewItemText topic =
  div
    [ style "flex" "auto"
    , style "overflow" "hidden"
    , style "text-overflow" "ellipsis"
    , style "white-space" "nowrap"
    ]
    [ text <| Topic.label topic ]


viewFullscreenButton : Id -> Model -> Html Msg
viewFullscreenButton id model =
  let
    isDisabled = model.boxId == id
  in
  case Topic.isBox id model of
    True ->
      button
        ( [ class "tool"
          , title "Fullscreen"
          , disabled isDisabled
          , U.onClickStop <| Search <| SearchDef.Fullscreen id -- don't trigger menu item
          , U.onPointerOverStop NoOp -- don't highlight menu item along with button
          , U.onPointerOutStop NoOp -- don't highlight menu item along with button
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


isItemDisabled : TopicId -> Model -> Bool
isItemDisabled topicId model =
  case TM.revelationBoxId model of
    Just boxId -> Box.hasDeepItem (toTopicId topicId) boxId model
    Nothing -> False


isTopicHover : TopicId -> Model -> Bool
isTopicHover topicId model =
  case model.search.result of
    Topics _ (Just topicId_) -> topicId_ == topicId
    _ -> False


isRelTopicHover : (TopicId, AssocId) -> Model -> Bool
isRelTopicHover relTopic model =
  case model.search.result of
    RelTopics _ (Just relTopic_) -> relTopic_ == relTopic
    _ -> False



-- UPDATE


update : SearchDef.Msg -> Env -> (UndoModel, Cmd Msg)
update msg ({model, undoModel} as env) =
  case msg of
    -- Search
    SearchDef.Input term -> (setSearchTerm term model, Cmd.none) |> Undo.swap undoModel
    SearchDef.InputFocused -> (onInputFocused model, Cmd.none) |> Undo.swap undoModel
    SearchDef.TopicHovered topicId -> (onTopicHovered topicId env, Cmd.none)
      |> Undo.swap undoModel
    SearchDef.TopicUnhovered _ -> (onTopicUnhovered env, Cmd.none) |> Undo.swap undoModel
    SearchDef.TopicClicked topicId -> revealTopic topicId env |> S.store |> Undo.push undoModel
    -- Traverse
    SearchDef.RelTopicHovered relTopicId -> (onRelTopicHovered relTopicId env, Cmd.none)
      |> Undo.swap undoModel
    SearchDef.RelTopicUnhovered _ -> (onRelTopicUnhovered env, Cmd.none) |> Undo.swap undoModel
    SearchDef.RelTopicClicked relTopicId -> revealRelTopic relTopicId env |> S.store
      |> Undo.push undoModel
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


onTopicHovered : TopicId -> Env -> Model
onTopicHovered topicId ({model} as env) =
  case model.search.result of
    Topics topicIds _ ->
      -- update hover state
      model
        |> setResult (Topics topicIds <| Just topicId)
        |> Env.autoSize env
    _ ->
      U.logError "Search.onTopicHovered" "search.result is not Topics" model


onRelTopicHovered : (TopicId, AssocId) -> Env -> Model
onRelTopicHovered relTopicId ({model} as env) =
  case model.search.result of
    RelTopics relTopicIds _ ->
      -- update hover state
      model
        |> setResult (RelTopics relTopicIds <| Just relTopicId)
        |> Env.autoSize env
    _ ->
      U.logError "Search.onRelTopicHovered" "search.result is not RelTopics" model


onTopicUnhovered : Env -> Model
onTopicUnhovered ({model} as env) =
  case model.search.result of
    Topics topicIds _ ->
      -- update hover state
      model
        |> setResult (Topics topicIds Nothing)
        |> Env.autoSize env
    _ ->
      U.logError "Search.onTopicUnhovered" "search.result is not Topics" model


onRelTopicUnhovered : Env -> Model
onRelTopicUnhovered ({model} as env) =
  case model.search.result of
    RelTopics relTopicIds _ ->
      -- update hover state
      model
        |> setResult (RelTopics relTopicIds Nothing)
        |> Env.autoSize env
    _ ->
      U.logError "Search.onRelTopicUnhovered" "search.result is not RelTopics" model


revealTopic : TopicId -> Env -> Model
revealTopic topicId ({model} as env) =
  case TM.revelationBoxPath model of
    Just (boxId :: _ as boxPath) ->
      model
        |> revealTopic_ topicId boxId
        |> closeMenu
        |> Sel.select (T topicId) boxPath
        |> Env.autoSize env
    _ -> model


revealRelTopic : (TopicId, AssocId) -> Env -> Model
revealRelTopic (topicId, assocId) ({model} as env) =
  case TM.revelationBoxPath model of
    Just (boxId :: _ as boxPath) ->
      model
        |> revealTopic_ topicId boxId
        |> revealAssoc_ assocId boxId
        |> closeMenu
        |> Sel.select (T topicId) boxPath
        |> Env.autoSize env
    _ -> model


revealTopic_ : TopicId -> BoxId -> Model -> Model
revealTopic_ topicId boxId model =
  model
    |> Box.addTopic (BoxTopic topicId Collapsed) boxId
    |> TM.addTopic topicId boxId Default -- TODO: let ExtManager dispatch instead
    |> Tuple.first -- Note: Cmd is ignored, OK for the moment ;-)


revealAssoc_ : AssocId -> BoxId -> Model -> Model
revealAssoc_ assocId boxId model =
  model
    |> Box.addAssoc assocId boxId


-- "searchTopics" instead "search" avoids shadowing
searchTopics : Model -> Model
searchTopics model =
  let
    topicIds = model.topics |> Dict.foldr
      (\id {text} idAcc ->
        if isMatch text model.search.term then
          (TopicId id) :: idAcc
        else
          idAcc
      )
      []
  in
  model
    |> setResult (Topics topicIds Nothing)


traverse : Model -> Model
traverse model =
  let
    relTopicIds =
      case Sel.single model of
        Just (T id, _) ->
          Assoc.relatedTopics id model
        _ -> [] -- TODO: log error
  in
  model
    |> setResult (RelTopics relTopicIds Nothing)


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
