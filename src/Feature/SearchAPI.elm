module Feature.SearchAPI exposing (viewInput, viewSearchResult, viewTraversalResult, closeMenu,
  update)

import Box
import Box.Size as Size
import Config as C
import Feature.IconAPI as IconAPI
import Feature.Nav as Nav
import Feature.NavAPI as NavAPI
import Feature.Search as Search exposing (Menu(..))
import Feature.SelAPI as SelAPI
import Item
import Model exposing (Model, Msg(..))
import ModelParts exposing (..)
import Storage as S
import Undo exposing (UndoModel)
import Utils as U

import Dict
import Html exposing (Html, Attribute, button, div, text, input)
import Html.Attributes exposing (attribute, class, style, title, value)
import Html.Events exposing (onClick, onInput, onFocus, on)
import Json.Decode as D
import String exposing (fromInt)



-- VIEW


viewInput : Model -> Html Msg
viewInput model =
  div
    []
    [ input
      ( [ value model.search.text
        , onInput (Search << Search.Input)
        , onFocus (Search Search.FocusInput)
        ]
        ++ searchInputStyle
      )
      []
    ]


searchInputStyle : List (Attribute Msg)
searchInputStyle =
  [ style "width" "100px" ]


viewSearchResult : Model -> List (Html Msg)
viewSearchResult model =
  case model.search.menu of
    Topics topicIds _ ->
      if not (topicIds |> List.isEmpty) then
        [ viewSearchtMenu topicIds model ]
      else
        []
    _ -> []


viewTraversalResult : Model -> List (Html Msg)
viewTraversalResult model =
  case model.search.menu of
    RelTopics relTopicIds _ ->
      if not (relTopicIds |> List.isEmpty) then
        [ viewTraversalMenu relTopicIds model ]
      else
        []
    _ -> []


viewSearchtMenu : List Id -> Model -> Html Msg
viewSearchtMenu topicIds model =
  div
    ( [ on "click" (topicDecoder Search.ClickTopic)
      , on "mouseover" (topicDecoder Search.HoverTopic)
      , on "mouseout" (topicDecoder Search.UnhoverTopic)
      , U.stopPropagationOnMousedown NoOp
      ]
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
              ( [ attribute "data-id" (fromInt id) ]
                ++ menuItemStyle isDisabled isHover
              )
              [ text topic.text
              , viewFullscreenButton id model
              ]
          Nothing -> text "??"
      )
    )


viewTraversalMenu : List (Id, Id) -> Model -> Html Msg
viewTraversalMenu relTopicIds model =
  div
    ( [ on "click" (relTopicDecoder Search.ClickRelTopic)
      , on "mouseover" (relTopicDecoder Search.HoverRelTopic)
      , on "mouseout" (relTopicDecoder Search.UnhoverRelTopic)
      , U.stopPropagationOnMousedown NoOp
      ]
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
              ( [ attribute "data-id" <| fromInt id ++ "," ++ fromInt assocId ]
                ++ menuItemStyle isDisabled isHover
              )
              [ text topic.text
              , viewFullscreenButton id model
              ] -- TODO: render assoc info
          Nothing -> text "??"
      )
    )


viewFullscreenButton : Id -> Model -> Html Msg
viewFullscreenButton id model =
  case Item.isBox id model of
    True ->
      button
      ( [ class "tool"
        , title "Fullscreen"
        , onClick <| Search <| Search.Fullscreen id
        ]
        ++ fullscreenButtonStyle
      )
      [ IconAPI.viewIcon "maximize-2" 16 ]
    False -> text ""


fullscreenButtonStyle : List (Attribute Msg)
fullscreenButtonStyle =
  [ style "border" "none"
  , style "background-color" "transparent"
  ]


isItemDisabled : Id -> Model -> Bool
isItemDisabled topicId model =
  case revealBoxId model of
    Just boxId -> Box.hasDeepItem topicId boxId model
    Nothing -> False


isTopicHover : Id -> Model -> Bool
isTopicHover topicId model =
  case model.search.menu of
    Topics _ (Just topicId_) -> topicId_ == topicId
    _ -> False


isRelTopicHover : (Id, Id) -> Model -> Bool
isRelTopicHover relTopic model =
  case model.search.menu of
    RelTopics _ (Just relTopic_) -> relTopic_ == relTopic
    _ -> False


{- The box where to reveal search/traversal results -}
revealBoxId : Model -> Maybe Id
revealBoxId model =
  case model.search.menu of
    Topics _ _ -> Just model.boxId
    RelTopics _ _ ->
      case SelAPI.singleBoxId model of
        Just boxId -> Just boxId
        Nothing -> Nothing
    Closed -> Nothing


searchResultStyle : List (Attribute Msg)
searchResultStyle =
  [ style "top" <| fromInt (C.appHeaderHeight - 5) ++ "px"
  , style "right" "20px"
  , style "z-index" "3" -- before topics (1,2)
  ]


traversalResultStyle : List (Attribute Msg)
traversalResultStyle =
  [ style "left" "65px" ]


menuStyle : List (Attribute Msg)
menuStyle =
  [ style "position" "absolute"
  , style "width" "210px"
  , style "padding" "3px 0"
  , style "font-size" <| fromInt C.contentFontSize ++ "px"
  , style "line-height" "2"
  , style "background-color" "white"
  , style "border" "1px solid lightgray"
  ]


menuItemStyle : Bool -> Bool -> List (Attribute Msg)
menuItemStyle isDisabled isHover =
  let
    (color, bgColor, pointerEvents) =
      if isDisabled then
        (C.disabledColor, "unset", "none")
      else
        ( "unset"
        , if isHover then C.hoverColor else "unset"
        , "unset"
        )
  in
  [ style "color" color
  , style "background-color" bgColor
  , style "overflow" "hidden"
  , style "text-overflow" "ellipsis"
  , style "padding" "0 8px"
  , style "pointer-events" pointerEvents
  ]


topicDecoder : (Id -> Search.Msg) -> D.Decoder Msg
topicDecoder msg =
  D.map Search <| D.map msg U.idDecoder


relTopicDecoder : ((Id, Id) -> Search.Msg) -> D.Decoder Msg
relTopicDecoder msg =
  D.map Search <| D.map msg U.idTupleDecoder



-- UPDATE


update : Search.Msg -> UndoModel -> (UndoModel, Cmd Msg)
update msg ({present} as undoModel) =
  case msg of
    -- Search
    Search.Input text -> (onTextInput text present, Cmd.none) |> Undo.swap undoModel
    Search.FocusInput -> (onFocusInput present, Cmd.none) |> Undo.swap undoModel
    Search.HoverTopic topicId -> (onHoverTopic topicId present, Cmd.none) |> Undo.swap undoModel
    Search.UnhoverTopic _ -> (onUnhoverTopic present, Cmd.none) |> Undo.swap undoModel
    Search.ClickTopic topicId -> revealTopic topicId present |> S.store |> Undo.push undoModel
    -- Traverse
    Search.Traverse -> (traverse present, Cmd.none) |> Undo.swap undoModel
    Search.HoverRelTopic relTopicId -> (onHoverRelTopic relTopicId present, Cmd.none)
      |> Undo.swap undoModel
    Search.UnhoverRelTopic _ -> (onUnhoverRelTopic present, Cmd.none) |> Undo.swap undoModel
    Search.ClickRelTopic relTopicId -> revealRelTopic relTopicId present |> S.store
      |> Undo.push undoModel
    -- Fullscreen
    Search.Fullscreen boxId -> fullscreen boxId present |> Undo.swap undoModel


onTextInput : String -> Model -> Model
onTextInput text ({search} as model) =
  { model | search = { search | text = text }}
  |> searchTopics


onFocusInput : Model -> Model
onFocusInput = searchTopics


onHoverTopic : Id -> Model -> Model
onHoverTopic topicId ({search} as model) =
  case model.search.menu of
    Topics topicIds _ ->
      -- update hover state
      { model | search = { search | menu = Topics topicIds (Just topicId) }}
    _ ->
      U.logError "onHoverTopic" "Received \"HoverTopic\" when search.menu is not Topics" model


onHoverRelTopic : (Id, Id) -> Model -> Model
onHoverRelTopic relTopicId ({search} as model) =
  case model.search.menu of
    RelTopics relTopicIds _ ->
      -- update hover state
      { model | search = { search | menu = RelTopics relTopicIds (Just relTopicId) }}
    _ ->
      U.logError "onHoverRelTopic"
        "Received \"HoverRelTopic\" when search.menu is not RelTopics" model


onUnhoverTopic : Model -> Model
onUnhoverTopic ({search} as model) =
  case model.search.menu of
    Topics topicIds _ ->
      -- update hover state
      { model | search = { search | menu = Topics topicIds Nothing }}
    _ ->
      U.logError "onUnhoverTopic"
        "Received \"UnhoverTopic\" when search.menu is not Topics"
        model


onUnhoverRelTopic : Model -> Model
onUnhoverRelTopic ({search} as model) =
  case model.search.menu of
    RelTopics relTopicIds _ ->
      -- update hover state
      { model | search = { search | menu = RelTopics relTopicIds Nothing }}
    _ ->
      U.logError "onUnhoverRelTopic"
        "Received \"UnhoverRelTopic\" when search.menu is not RelTopics" model


revealTopic : Id -> Model -> Model
revealTopic topicId model =
  model
  |> revealItem topicId model.boxId
  |> closeMenu


revealRelTopic : (Id, Id) -> Model -> Model
revealRelTopic (topicId, assocId) model =
  case SelAPI.singleBoxId model of
    Just boxId ->
      model
      |> revealItem topicId boxId
      |> revealItem assocId boxId
      |> closeMenu
      |> Size.auto
    Nothing -> model


fullscreen : BoxId -> Model -> (Model, Cmd Msg)
fullscreen boxId model =
  ( model |> closeMenu
  , NavAPI.pushUrl boxId model
  )


searchTopics : Model -> Model
searchTopics ({search} as model) =
  let
    topicIds = model.items |> Dict.foldr
      (\id item topicIdsAcc ->
        case item.info of
          Topic {text} ->
            if isMatch model.search.text text then
              id :: topicIdsAcc
            else
              topicIdsAcc
          Assoc _ -> topicIdsAcc
      )
      []
  in
  { model | search = { search | menu = Topics topicIds Nothing }}


traverse : Model -> Model
traverse ({search} as model) =
  let
    relTopicIds =
      case SelAPI.single model of
        Just (itemId, _) ->
          Item.relatedItems itemId model
        Nothing -> [] -- TODO: log error
  in
  { model | search = { search | menu = RelTopics relTopicIds Nothing }}


isMatch : String -> String -> Bool
isMatch searchText text =
  not (searchText |> String.isEmpty)
  && String.contains (String.toLower searchText) (String.toLower text)


revealItem : Id -> BoxId -> Model -> Model
revealItem itemId boxId model =
  if Box.hasItem boxId itemId model then
    let
      _ = U.info "revealItem" <| fromInt itemId ++ " is in " ++ fromInt boxId
    in
    Box.showItem itemId boxId model
  else
    let
      _ = U.info "revealItem" <| fromInt itemId ++ " not in " ++ fromInt boxId
      props = Box.initItemProps itemId boxId model
    in
    Box.addItem itemId props boxId model


closeMenu : Model -> Model
closeMenu ({search} as model) =
  { model | search = { search | menu = Closed }}
