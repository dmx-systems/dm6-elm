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
import Outcome exposing (..)
import Shared.Events as Events
import Topic
import TopicMap.TopicMap as TopicMap
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
    target = TopicMap.landingTarget model
  in
  div
    []
    [ input
      ( [ value model.search.term
        , onInput (Search << SearchDef.Input)
        , onFocus (Search SearchDef.InputFocused)
        , Events.onPointerDownStop <| Cancel target -- Don't clear selection
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
    ( [ Events.onPointerDownStop NoOp ] -- Prevent search menu closing
      ++ searchResultStyle
      ++ menuStyle
    )
    (topicIds |> List.map
      (\topicId ->
        let
          isDisabled = isItemDisabled topicId model
          isHover = isTopicHover topicId model
        in
        case Topic.fromId topicId model of
          Just topic ->
            div
              ( [ onClick <| Search <| SearchDef.TopicClicked topicId
                , on "pointerover" <| D.succeed <| Search <| SearchDef.TopicHovered topicId
                , on "pointerout" <| D.succeed <| Search <| SearchDef.TopicUnhovered topicId
                ]
                ++ menuItemStyle isDisabled isHover
              )
              [ viewTopicIcon topic model
              , viewItemText topic
              , viewFullscreenButton topicId model
              ]
          Nothing -> text "??"
      )
    )


viewTraversalMenu : List (TopicId, AssocId) -> Model -> Html Msg
viewTraversalMenu relTopicIds model =
  div
    ( [ Events.onPointerDownStop NoOp ] -- Prevent traversal menu closing
      ++ traversalResultStyle
      ++ menuStyle
    )
    (relTopicIds |> List.map
      (\((topicId, assocId) as relTopic) ->
        let
          isDisabled = isItemDisabled topicId model
          isHover = isRelTopicHover relTopic model
        in
        case Topic.fromId topicId model of
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
              , viewFullscreenButton topicId model
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


viewFullscreenButton : TopicId -> Model -> Html Msg
viewFullscreenButton topicId model =
  let
    isDisabled = fromBoxId model.boxId == topicId
  in
  if Topic.isBox topicId model then
    button
      ( [ class "tool"
        , title "Fullscreen"
        , disabled isDisabled      -- don't trigger menu item ↓
        , Events.onClickStop <| Search <| SearchDef.Fullscreen (BoxId topicId)
        , Events.onPointerOverStop NoOp -- don't highlight menu item along with button
        , Events.onPointerOutStop NoOp -- don't highlight menu item along with button
        ]
        ++ fullscreenButtonStyle
      )
      [ Icon.view "maximize-2" 16 [] ]
  else
    text ""


fullscreenButtonStyle : Attrs Msg
fullscreenButtonStyle =
  [ style "border" "none"
  , style "background-color" "transparent"
  , style "pointer-events" "initial"
  ]


isItemDisabled : TopicId -> Model -> Bool
isItemDisabled topicId model =
  case TopicMap.revelationBoxId model of
    Just (BoxId topicId_) -> Box.hadDeepTopic topicId_ topicId model
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


update : SearchDef.Msg -> Env -> Outcome
update msg env =
  case msg of
    -- Search
    SearchDef.Input term ->
      env
        |> Env.map (setSearchTerm term)
        |> Env.outcome
    SearchDef.InputFocused ->
      env
        |> Env.map onInputFocused
        |> Env.outcome
    SearchDef.TopicHovered topicId ->
      env
        |> onTopicHovered topicId
        |> Env.outcome
    SearchDef.TopicUnhovered _ ->
      env
        |> onTopicUnhovered
        |> Env.outcome
    SearchDef.TopicClicked topicId ->
      env
        |> revealTopic topicId
        |> Env.outcomeWith (Directives Store Push)
    -- Traverse
    SearchDef.RelTopicHovered relTopicId ->
      env
        |> onRelTopicHovered relTopicId
        |> Env.outcome
    SearchDef.RelTopicUnhovered _ ->
      env
        |> onRelTopicUnhovered
        |> Env.outcome
    SearchDef.RelTopicClicked relTopicId ->
      env
        |> revealRelTopic relTopicId
        |> Env.outcomeWith (Directives Store Push)
    -- Fullscreen (Search & Traverse)
    SearchDef.Fullscreen boxId ->
      env
        |> Env.outcomeWithCmd (Nav.pushUrl boxId)


setSearchTerm : String -> Model -> Model
setSearchTerm term model =
  model
    |> setTerm term
    |> searchTopics


onInputFocused : Model -> Model
onInputFocused =
  searchTopics


onTopicHovered : TopicId -> Env -> Env
onTopicHovered topicId ({model} as env) =
  case model.search.result of
    Topics topicIds _ ->
      env
        |> Env.map (setResult (Topics topicIds (Just topicId))) -- update hover state
        |> Env.autoSize
    _ ->
      let
        _ = U.logError "Feature.Search.onTopicHovered"
          "For model.search.result Topics is expected but is" model.search.result
      in
      env


onRelTopicHovered : (TopicId, AssocId) -> Env -> Env
onRelTopicHovered relTopicId ({model} as env) =
  case model.search.result of
    RelTopics relTopicIds _ ->
      env
        |> Env.map (setResult (RelTopics relTopicIds (Just relTopicId))) -- update hover state
        |> Env.autoSize
    _ ->
      let
        _ = U.logError "Feature.Search.onRelTopicHovered"
          "For model.search.result RelTopics is expected but is" model.search.result
      in
      env


onTopicUnhovered : Env -> Env
onTopicUnhovered ({model} as env) =
  case model.search.result of
    Topics topicIds _ ->
      env
        |> Env.map (setResult (Topics topicIds Nothing)) -- update hover state
        |> Env.autoSize
    _ ->
      let
        _ = U.logError "Feature.Search.onTopicUnhovered"
          "For model.search.result Topics is expected but is" model.search.result
      in
      env


onRelTopicUnhovered : Env -> Env
onRelTopicUnhovered ({model} as env) =
  case model.search.result of
    RelTopics relTopicIds _ ->
      env
        |> Env.map (setResult (RelTopics relTopicIds Nothing)) -- update hover state
        |> Env.autoSize
    _ ->
      let
        _ = U.logError "Feature.Search.onRelTopicUnhovered"
          "For model.search.result RelTopics is expected but is" model.search.result
      in
      env


revealTopic : TopicId -> Env -> Env
revealTopic topicId ({model} as env) =
  case TopicMap.revelationBoxPath model of
    Just (boxId :: _ as boxPath) ->
      env
        |> revealTopic_ topicId boxId
        |> Env.map closeMenu
        |> Env.map (Sel.select (T topicId) boxPath)
        |> Env.autoSize
    _ ->
      env


revealRelTopic : (TopicId, AssocId) -> Env -> Env
revealRelTopic (topicId, assocId) ({model} as env) =
  case TopicMap.revelationBoxPath model of
    Just (boxId :: _ as boxPath) ->
      env
        |> revealTopic_ topicId boxId
        |> Env.map (revealAssoc_ assocId boxId)
        |> Env.map closeMenu
        |> Env.map (Sel.select (T topicId) boxPath)
        |> Env.autoSize
    _ -> env


revealTopic_ : TopicId -> BoxId -> Env -> Env
revealTopic_ topicId boxId env =
  env
    |> Box.addTopic (BoxTopic topicId Collapsed) boxId


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
  model
    |> setResult NoSearch


setTerm : String -> Model -> Model
setTerm term ({search} as model) =
  { model | search = { search | term = term }}


setResult : SearchResult -> Model -> Model
setResult result ({search} as model) =
  { model | search = { search | result = result }}
