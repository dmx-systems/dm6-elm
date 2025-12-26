module Feature.SearchAPI exposing (viewInput, viewSearchResult, viewTraversalResult, traverse,
  closeMenu, update)

import Box
import Box.Size as Size
import Config as C
import Feature.IconAPI as IconAPI
import Feature.NavAPI as NavAPI
import Feature.Search as Search exposing (SearchResult(..))
import Feature.SelAPI as SelAPI
import Item
import Model exposing (Model, Msg(..))
import ModelParts exposing (..)
import Storage as S
import Undo exposing (UndoModel)
import Utils as U

import Dict
import Html exposing (Html, button, div, input, text)
import Html.Attributes exposing (class, disabled, style, title, value)
import Html.Events exposing (onClick, onMouseOver, onMouseOut, onInput, onFocus)
import String exposing (fromInt)



-- VIEW


viewInput : Model -> Html Msg
viewInput model =
  div
    []
    [ input
      ( [ value model.search.term
        , onInput (Search << Search.Input)
        , onFocus (Search Search.InputFocused)
        , U.onMouseDownStop NoOp -- Don't clear selection
        ]
        ++ searchInputStyle
      )
      []
    ]


searchInputStyle : Attributes Msg
searchInputStyle =
  [ style "width" "100px" ]


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
              ( [ onClick <| Search <| Search.TopicClicked id
                , onMouseOver <| Search <| Search.TopicHovered id
                , onMouseOut <| Search <| Search.TopicUnhovered id
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
              ( [ onClick <| Search <| Search.RelTopicClicked (id, assocId)
                , onMouseOver <| Search <| Search.RelTopicHovered (id, assocId)
                , onMouseOut <| Search <| Search.RelTopicUnhovered (id, assocId)
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


searchResultStyle : Attributes Msg
searchResultStyle =
  [ style "top" <| fromInt (C.appHeaderHeight - 5) ++ "px"
  , style "right" "20px"
  , style "z-index" "3" -- before topics (1,2)
  ]


traversalResultStyle : Attributes Msg
traversalResultStyle =
  [ style "left" "65px" ]


menuStyle : Attributes Msg
menuStyle =
  [ style "position" "absolute"
  , style "width" "210px"
  , style "padding" "3px 0"
  , style "font-size" <| fromInt C.contentFontSize ++ "px"
  , style "line-height" "2"
  , style "background-color" "white"
  , style "border" "1px solid lightgray"
  ]


menuItemStyle : Bool -> Bool -> Attributes Msg
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
  IconAPI.viewTopicIcon topic.id C.topicIconSize [ style "flex" "none" ] model


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
          , U.onClickStop <| Search <| Search.Fullscreen id -- don't trigger menu item
          , U.onMouseOverStop NoOp -- don't highlight menu item along with button
          , U.onMouseOutStop NoOp -- don't highlight menu item along with button
          ]
          ++ fullscreenButtonStyle
        )
        [ IconAPI.view "maximize-2" 16 [] ]
    False -> text ""


fullscreenButtonStyle : Attributes Msg
fullscreenButtonStyle =
  [ style "border" "none"
  , style "background-color" "transparent"
  , style "pointer-events" "initial"
  ]


isItemDisabled : Id -> Model -> Bool
isItemDisabled topicId model =
  case SelAPI.revelationBoxId model of
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


update : Search.Msg -> UndoModel -> (UndoModel, Cmd Msg)
update msg ({present} as undoModel) =
  case msg of
    -- Search
    Search.Input term -> (setSearchTerm term present, Cmd.none) |> Undo.swap undoModel
    Search.InputFocused -> (onInputFocused present, Cmd.none) |> Undo.swap undoModel
    Search.TopicHovered topicId -> (onTopicHovered topicId present, Cmd.none)
      |> Undo.swap undoModel
    Search.TopicUnhovered _ -> (onTopicUnhovered present, Cmd.none) |> Undo.swap undoModel
    Search.TopicClicked topicId -> revealTopic topicId present |> S.store |> Undo.push undoModel
    -- Traverse
    Search.RelTopicHovered relTopicId -> (onRelTopicHovered relTopicId present, Cmd.none)
      |> Undo.swap undoModel
    Search.RelTopicUnhovered _ -> (onRelTopicUnhovered present, Cmd.none) |> Undo.swap undoModel
    Search.RelTopicClicked relTopicId -> revealRelTopic relTopicId present |> S.store
      |> Undo.push undoModel
    -- Fullscreen (Search & Traverse)
    Search.Fullscreen boxId -> (undoModel, NavAPI.pushUrl boxId)


setSearchTerm : String -> Model -> Model
setSearchTerm term model =
  model
    |> setTerm term
    |> searchTopics


onInputFocused : Model -> Model
onInputFocused =
  searchTopics


onTopicHovered : Id -> Model -> Model
onTopicHovered topicId model =
  case model.search.result of
    Topics topicIds _ ->
      -- update hover state
      model
        |> setResult (Topics topicIds <| Just topicId)
        |> Size.auto
    _ ->
      U.logError "onTopicHovered" "search.result is not Topics" model


onRelTopicHovered : (Id, Id) -> Model -> Model
onRelTopicHovered relTopicId model =
  case model.search.result of
    RelTopics relTopicIds _ ->
      -- update hover state
      model
        |> setResult (RelTopics relTopicIds <| Just relTopicId)
        |> Size.auto
    _ ->
      U.logError "onRelTopicHovered" "search.result is not RelTopics" model


onTopicUnhovered : Model -> Model
onTopicUnhovered model =
  case model.search.result of
    Topics topicIds _ ->
      -- update hover state
      model
        |> setResult (Topics topicIds Nothing)
        |> Size.auto
    _ ->
      U.logError "onTopicUnhovered" "search.result is not Topics" model


onRelTopicUnhovered : Model -> Model
onRelTopicUnhovered model =
  case model.search.result of
    RelTopics relTopicIds _ ->
      -- update hover state
      model
        |> setResult (RelTopics relTopicIds Nothing)
        |> Size.auto
    _ ->
      U.logError "onRelTopicUnhovered" "search.result is not RelTopics" model


revealTopic : Id -> Model -> Model
revealTopic topicId model =
  case SelAPI.revelationBoxPath model of
    Just (boxId :: _ as boxPath) ->
      model
        |> Box.revealItem topicId boxId
        |> closeMenu
        |> SelAPI.select topicId boxPath
        |> Size.auto
    _ -> model


revealRelTopic : (Id, Id) -> Model -> Model
revealRelTopic (topicId, assocId) model =
  case SelAPI.revelationBoxPath model of
    Just (boxId :: _ as boxPath) ->
      model
        |> Box.revealItem topicId boxId
        |> Box.revealItem assocId boxId
        |> closeMenu
        |> SelAPI.select topicId boxPath
        |> Size.auto
    _ -> model


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
      case SelAPI.single model of
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
