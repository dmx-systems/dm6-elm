module SearchAPI exposing (viewSearchInput, viewResultMenu, closeResultMenu, updateSearch)

import AppModel exposing (UndoModel, Model, Msg(..))
import Config as C
import MapAutoSize exposing (autoSize)
import Model exposing (ItemInfo(..), Id, BoxId)
import ModelAPI exposing (topicById, relatedItems, activeMap, initItemProps, isItemInMap,
  isItemInMapDeep, showItem, putItemOnMap, singleSelection, singleSelectionMapId, push, swap)
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
        let
          isDisabled = isItemDisabled id model
          isHover = isTopicHover id model
        in
        case topicById id model of
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
      , stopPropagationOnMousedown NoOp
      ]
      ++ resultMenuStyle
    )
    (relTopicIds |> List.map
      (\((id, assocId) as relTopic) ->
        let
          isDisabled = isItemDisabled id model
          isHover = isRelTopicHover relTopic model
        in
        case topicById id model of
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
  case revealMapId model of
    Just boxId -> isItemInMapDeep boxId topicId model
    Nothing -> False


{- The box where to reveal search/related results -}
revealMapId : Model -> Maybe Id
revealMapId model =
  case model.search.menu of
    Topics _ _ -> Just (activeMap model)
    RelTopics _ _ ->
      case singleSelectionMapId model of
        Just boxId -> Just boxId
        Nothing -> Nothing
    Closed -> Nothing


topicDecoder : (Id -> Search.Msg) -> D.Decoder Msg
topicDecoder msg =
  D.map Search <| D.map msg idDecoder


relTopicDecoder : ((Id, Id) -> Search.Msg) -> D.Decoder Msg
relTopicDecoder msg =
  D.map Search <| D.map msg idTupleDecoder


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


updateSearch : Search.Msg -> UndoModel -> (UndoModel, Cmd Msg)
updateSearch msg ({present} as undoModel) =
  case msg of
    Search.Input text -> (onTextInput text present, Cmd.none) |> swap undoModel
    Search.FocusInput -> (onFocusInput present, Cmd.none) |> swap undoModel
    Search.HoverTopic topicId -> (onHoverTopic topicId present, Cmd.none) |> swap undoModel
    Search.UnhoverTopic _ -> (onUnhoverTopic present, Cmd.none) |> swap undoModel
    Search.ClickTopic topicId -> revealTopic topicId present |> store |> push undoModel
    -- Traverse
    Search.ShowRelated -> (showRelatedTopics present, Cmd.none) |> swap undoModel
    Search.HoverRelTopic relTopicId -> (onHoverRelTopic relTopicId present, Cmd.none)
      |> swap undoModel
    Search.UnhoverRelTopic _ -> (onUnhoverRelTopic present, Cmd.none) |> swap undoModel
    Search.ClickRelTopic relTopicId -> revealRelTopic relTopicId present |> store
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


revealTopic : Id -> Model -> Model
revealTopic topicId model =
  model
  |> revealItem topicId (activeMap model)
  |> closeResultMenu


revealRelTopic : (Id, Id) -> Model -> Model
revealRelTopic (topicId, assocId) model =
  case singleSelectionMapId model of
    Just boxId ->
      model
      |> revealItem topicId boxId
      |> revealItem assocId boxId
      |> closeResultMenu
      |> autoSize
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
      case singleSelection model of
        Just (itemId, _) ->
          relatedItems itemId model
        Nothing -> [] -- TODO: log error
  in
  { model | search = { search | menu = RelTopics relTopicIds Nothing }}


isMatch : String -> String -> Bool
isMatch searchText text =
  not (searchText |> String.isEmpty)
  && String.contains (String.toLower searchText) (String.toLower text)


revealItem : Id -> BoxId -> Model -> Model
revealItem itemId boxId model =
  if isItemInMap itemId boxId model then
    let
      _ = info "revealItem" <| fromInt itemId ++ " is in " ++ fromInt boxId
    in
    showItem itemId boxId model
  else
    let
      _ = info "revealItem" <| fromInt itemId ++ " not in " ++ fromInt boxId
      props = initItemProps itemId model
    in
    putItemOnMap itemId props boxId model


closeResultMenu : Model -> Model
closeResultMenu ({search} as model) =
  { model | search = { search | menu = Closed }}
