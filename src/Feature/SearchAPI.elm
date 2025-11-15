module Feature.SearchAPI exposing (viewInput, viewMenu, closeMenu, update)

import AppModel exposing (..)
import AutoSize as Size
import Config as C
import Model exposing (..)
import ModelAPI as A
import Storage as S
import Utils as U
-- feature modules
import Feature.Search as Search exposing (Menu(..))

import Dict
import Html exposing (Html, Attribute, div, text, input)
import Html.Attributes exposing (value, style, attribute)
import Html.Events exposing (onInput, onFocus, on)
import Json.Decode as D
import String exposing (fromInt)



-- VIEW


viewInput : Model -> Html Msg
viewInput model =
  div
    []
    [ div
      []
      [ text "Search" ]
    , input
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


viewMenu : Model -> List (Html Msg)
viewMenu model =
  case model.search.menu of
    Topics topicIds _ ->
      if not (topicIds |> List.isEmpty) then
        [ viewTopicsMenu topicIds model ]
      else
        []
    RelTopics relTopicIds _ ->
      if not (relTopicIds |> List.isEmpty) then
        [ viewRelTopicsMenu relTopicIds model ]
      else
        []
    Closed -> []


viewTopicsMenu : List Id -> Model -> Html Msg
viewTopicsMenu topicIds model =
  div
    ( [ on "click" (topicDecoder Search.ClickTopic)
      , on "mouseover" (topicDecoder Search.HoverTopic)
      , on "mouseout" (topicDecoder Search.UnhoverTopic)
      , U.stopPropagationOnMousedown NoOp
      ]
      ++ resultMenuStyle
    )
    (topicIds |> List.map
      (\id ->
        let
          isDisabled = isItemDisabled id model
          isHover = isTopicHover id model
        in
        case A.topicById id model of
          Just topic ->
            div
              ( [ attribute "data-id" (fromInt id) ]
                ++ resultItemStyle isDisabled isHover
              )
              [ text topic.text ]
          Nothing -> text "??"
      )
    )


viewRelTopicsMenu : List (Id, Id) -> Model -> Html Msg
viewRelTopicsMenu relTopicIds model =
  div
    ( [ on "click" (relTopicDecoder Search.ClickRelTopic)
      , on "mouseover" (relTopicDecoder Search.HoverRelTopic)
      , on "mouseout" (relTopicDecoder Search.UnhoverRelTopic)
      , U.stopPropagationOnMousedown NoOp
      ]
      ++ resultMenuStyle
    )
    (relTopicIds |> List.map
      (\((id, assocId) as relTopic) ->
        let
          isDisabled = isItemDisabled id model
          isHover = isRelTopicHover relTopic model
        in
        case A.topicById id model of
          Just topic ->
            div
              ( [ attribute "data-id" <| fromInt id ++ "," ++ fromInt assocId ]
                ++ resultItemStyle isDisabled isHover
              )
              [ text topic.text ] -- TODO: render assoc info
          Nothing -> text "??"
      )
    )


isItemDisabled : Id -> Model -> Bool
isItemDisabled topicId model =
  case revealBoxId model of
    Just boxId -> A.boxHasDeepItem topicId boxId model
    Nothing -> False


{- The box where to reveal search/related results -}
revealBoxId : Model -> Maybe Id
revealBoxId model =
  case model.search.menu of
    Topics _ _ -> Just (A.activeBox model)
    RelTopics _ _ ->
      case A.singleSelectionBoxId model of
        Just boxId -> Just boxId
        Nothing -> Nothing
    Closed -> Nothing


topicDecoder : (Id -> Search.Msg) -> D.Decoder Msg
topicDecoder msg =
  D.map Search <| D.map msg U.idDecoder


relTopicDecoder : ((Id, Id) -> Search.Msg) -> D.Decoder Msg
relTopicDecoder msg =
  D.map Search <| D.map msg U.idTupleDecoder


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


resultMenuStyle : List (Attribute Msg)
resultMenuStyle =
  [ style "position" "absolute"
  , style "top" "138px"
  , style "width" "240px"
  , style "padding" "3px 0"
  , style "font-size" <| fromInt C.contentFontSize ++ "px"
  , style "line-height" "2"
  , style "white-space" "nowrap"
  , style "background-color" "white"
  , style "border" "1px solid lightgray"
  , style "z-index" "2"
  ]


resultItemStyle : Bool -> Bool -> List (Attribute Msg)
resultItemStyle isDisabled isHover =
  let
    (color, bgColor, pointerEvents) =
      if isDisabled then
        (C.disabledColor, "unset", "none")
      else
        ( if isHover then "white" else "black"
        , if isHover then "black" else "white"
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



-- UPDATE


update : Search.Msg -> UndoModel -> (UndoModel, Cmd Msg)
update msg ({present} as undoModel) =
  case msg of
    Search.Input text -> (onTextInput text present, Cmd.none) |> A.swap undoModel
    Search.FocusInput -> (onFocusInput present, Cmd.none) |> A.swap undoModel
    Search.HoverTopic topicId -> (onHoverTopic topicId present, Cmd.none) |> A.swap undoModel
    Search.UnhoverTopic _ -> (onUnhoverTopic present, Cmd.none) |> A.swap undoModel
    Search.ClickTopic topicId -> revealTopic topicId present |> S.store |> A.push undoModel
    -- Traverse
    Search.ShowRelated -> (showRelatedTopics present, Cmd.none) |> A.swap undoModel
    Search.HoverRelTopic relTopicId -> (onHoverRelTopic relTopicId present, Cmd.none)
      |> A.swap undoModel
    Search.UnhoverRelTopic _ -> (onUnhoverRelTopic present, Cmd.none) |> A.swap undoModel
    Search.ClickRelTopic relTopicId -> revealRelTopic relTopicId present |> S.store
      |> A.push undoModel


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
        "Received \"HoverRelTopic\" when search.menu is not RelTopics"
        model


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
  |> revealItem topicId (A.activeBox model)
  |> closeMenu


revealRelTopic : (Id, Id) -> Model -> Model
revealRelTopic (topicId, assocId) model =
  case A.singleSelectionBoxId model of
    Just boxId ->
      model
      |> revealItem topicId boxId
      |> revealItem assocId boxId
      |> closeMenu
      |> Size.auto
    Nothing -> model


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


showRelatedTopics : Model -> Model
showRelatedTopics ({search} as model) =
  let
    relTopicIds =
      case A.singleSelection model of
        Just (itemId, _) ->
          A.relatedItems itemId model
        Nothing -> [] -- TODO: log error
  in
  { model | search = { search | menu = RelTopics relTopicIds Nothing }}


isMatch : String -> String -> Bool
isMatch searchText text =
  not (searchText |> String.isEmpty)
  && String.contains (String.toLower searchText) (String.toLower text)


revealItem : Id -> BoxId -> Model -> Model
revealItem itemId boxId model =
  if A.boxHasItem boxId itemId model then
    let
      _ = U.info "revealItem" <| fromInt itemId ++ " is in " ++ fromInt boxId
    in
    A.showItem itemId boxId model
  else
    let
      _ = U.info "revealItem" <| fromInt itemId ++ " not in " ++ fromInt boxId
      props = A.initItemProps itemId boxId model
    in
    A.addItemToBox itemId props boxId model


closeMenu : Model -> Model
closeMenu ({search} as model) =
  { model | search = { search | menu = Closed }}
