module SearchAPI exposing (viewSearchInput, viewResultMenu, closeResultMenu, updateSearch)

import AppModel exposing (UndoModel, Model, Msg(..))
import Config exposing (contentFontSize, topicSize)
import Model exposing (ItemInfo(..), MapProps(..), Id, MapId)
import ModelAPI exposing (..)
import Storage exposing (store)
import Utils exposing (idDecoder, idTupleDecoder, stopPropagationOnMousedown, logError, info)
-- components
import Search exposing (Menu(..))

import Dict
import Html exposing (Html, Attribute, div, text, input)
import Html.Attributes exposing (value, style, attribute)
import Html.Events exposing (onInput, onFocus, on)
import Json.Decode as D
import String exposing (fromInt)



-- VIEW


viewSearchInput : Model -> Html Msg
viewSearchInput model =
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


viewResultMenu : Model -> List (Html Msg)
viewResultMenu model =
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
      , stopPropagationOnMousedown NoOp
      ]
      ++ resultMenuStyle
    )
    (topicIds |> List.map
      (\id ->
        case getTopicInfo id model of
          Just topic ->
            div
              ( [ attribute "data-id" (fromInt id) ]
                ++ resultItemStyle id model
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
      , stopPropagationOnMousedown NoOp
      ]
      ++ resultMenuStyle
    )
    (relTopicIds |> List.map
      (\(id, assocId) ->
        case getTopicInfo id model of
          Just topic ->
            div
              ( [ attribute "data-id" <| fromInt id ++ "," ++ fromInt assocId ]
                ++ resultItemStyle id model
              )
              [ text topic.text ]
          Nothing -> text "??"
      )
    )


topicDecoder : (Id -> Search.Msg) -> D.Decoder Msg
topicDecoder msg =
  D.map Search <| D.map msg idDecoder


relTopicDecoder : ((Id, Id) -> Search.Msg) -> D.Decoder Msg
relTopicDecoder msg =
  D.map Search <| D.map msg idTupleDecoder


resultMenuStyle : List (Attribute Msg)
resultMenuStyle =
  [ style "position" "absolute"
  , style "top" "144px"
  , style "width" "240px"
  , style "padding" "3px 0"
  , style "font-size" <| fromInt contentFontSize ++ "px"
  , style "line-height" "2"
  , style "white-space" "nowrap"
  , style "background-color" "white"
  , style "border" "1px solid lightgray"
  , style "z-index" "2"
  ]


resultItemStyle : Id -> Model -> List (Attribute Msg)
resultItemStyle topicId model =
  let
    isHover =
      case model.search.menu of
        Topics _ (Just topicId_) -> topicId_ == topicId
        _ -> False
  in
  [ style "color" (if isHover then "white" else "black")
  , style "background-color" (if isHover then "black" else "white")
  , style "overflow" "hidden"
  , style "text-overflow" "ellipsis"
  , style "padding" "0 8px"
  ]



-- UPDATE


updateSearch : Search.Msg -> UndoModel -> (UndoModel, Cmd Msg)
updateSearch msg ({present} as undoModel) =
  case msg of
    Search.Input text -> (onTextInput text present, Cmd.none) |> swap undoModel
    Search.FocusInput -> (onFocusInput present, Cmd.none) |> swap undoModel
    Search.HoverTopic topicId -> (onHoverTopic topicId present, Cmd.none) |> swap undoModel
    Search.UnhoverTopic _ -> (onUnhoverTopic present, Cmd.none) |> swap undoModel
    Search.ClickTopic topicId -> onClickTopic topicId present |> store |> push undoModel
    -- Traverse
    Search.ShowRelated -> (onShowRelated present, Cmd.none) |> swap undoModel
    Search.HoverRelTopic relTopicId -> (onHoverRelTopic relTopicId present, Cmd.none)
      |> swap undoModel
    Search.UnhoverRelTopic _ -> (onUnhoverRelTopic present, Cmd.none) |> swap undoModel
    Search.ClickRelTopic relTopicId -> onClickRelTopic relTopicId present |> store
      |> push undoModel


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
      logError "onHoverTopic" "Received \"HoverTopic\" when search.menu is not Topics" model


onHoverRelTopic : (Id, Id) -> Model -> Model
onHoverRelTopic relTopicId ({search} as model) =
  case model.search.menu of
    RelTopics relTopicIds _ ->
      -- update hover state
      { model | search = { search | menu = RelTopics relTopicIds (Just relTopicId) }}
    _ ->
      logError "onHoverRelTopic" "Received \"HoverRelTopic\" when search.menu is not RelTopics"
      model


onUnhoverTopic : Model -> Model
onUnhoverTopic ({search} as model) =
  case model.search.menu of
    Topics topicIds _ ->
      -- update hover state
      { model | search = { search | menu = Topics topicIds Nothing }}
    _ ->
      logError "onUnhoverTopic" "Received \"UnhoverTopic\" when search.menu is not Topics" model


onUnhoverRelTopic : Model -> Model
onUnhoverRelTopic ({search} as model) =
  case model.search.menu of
    RelTopics relTopicIds _ ->
      -- update hover state
      { model | search = { search | menu = RelTopics relTopicIds Nothing }}
    _ ->
      logError "onUnhoverRelTopic"
        "Received \"UnhoverRelTopic\" when search.menu is not RelTopics" model


onClickTopic : Id -> Model -> Model
onClickTopic topicId model =
  model
  |> revealTopic topicId (activeMap model)
  |> closeResultMenu


onClickRelTopic : (Id, Id) -> Model -> Model
onClickRelTopic (topicId, assocId) model =
  model
  |> revealTopic topicId (activeMap model) -- TODO: reveal assoc
  |> closeResultMenu


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


isMatch : String -> String -> Bool
isMatch searchText text =
  not (searchText |> String.isEmpty)
  && String.contains (String.toLower searchText) (String.toLower text)


revealTopic : Id -> MapId -> Model -> Model
revealTopic topicId mapId model =
  if isItemInMap topicId mapId model then
    let
      _ = info "revealTopic" (topicId, "set visible")
    in
    showItem topicId mapId model
  else
    let
      _ = info "revealTopic" (topicId, "add to map")
      props = MapTopic <| defaultProps topicId topicSize model
    in
    addItemToMap topicId props mapId model


closeResultMenu : Model -> Model
closeResultMenu ({search} as model) =
  { model | search = { search | menu = Closed }}


-- Traverse

onShowRelated : Model -> Model
onShowRelated ({search} as model) =
  let
    relTopicIds = [] -- TODO
  in
  { model | search = { search | menu = RelTopics relTopicIds Nothing }}
