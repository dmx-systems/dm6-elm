module SearchAPI exposing (viewSearchInput, viewResultMenu, closeResultMenu, updateSearch)

import AppModel exposing (UndoModel, Model, Msg(..))
import Config exposing (contentFontSize, topicSize)
import Model exposing (ItemInfo(..), MapProps(..), Id, MapId)
import ModelAPI exposing (..)
import Storage exposing (store)
import Utils exposing (idDecoder, stopPropagationOnMousedown, logError, info)
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
        [ div
          ( [ on "click" (itemDecoder Search.ClickItem)
            , on "mouseover" (itemDecoder Search.HoverItem)
            , on "mouseout" (itemDecoder Search.UnhoverItem)
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
        ]
      else
        []
    _ -> []


itemDecoder : (Id -> Search.Msg) -> D.Decoder Msg
itemDecoder msg =
  D.map Search <| D.map msg idDecoder


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
    Search.HoverItem topicId -> (onHoverItem topicId present, Cmd.none) |> swap undoModel
    Search.UnhoverItem _ -> (onUnhoverItem present, Cmd.none) |> swap undoModel
    Search.ClickItem topicId ->
      present
      |> revealTopic topicId (activeMap present)
      |> closeResultMenu
      |> store
      |> push undoModel


onTextInput : String -> Model -> Model
onTextInput text ({search} as model) =
  { model | search = { search | text = text }}
  |> searchTopics


onFocusInput : Model -> Model
onFocusInput = searchTopics


onHoverItem : Id -> Model -> Model
onHoverItem topicId ({search} as model) =
  case model.search.menu of
    Topics topicIds _ ->
      -- update hover state
      { model | search = { search | menu = Topics topicIds (Just topicId) }}
    Closed ->
      logError "onHoverItem" "Received \"HoverItem\" message when search.menu is Closed"
      model


onUnhoverItem : Model -> Model
onUnhoverItem ({search} as model) =
  case model.search.menu of
    Topics topicIds _ ->
      -- update hover state
      { model | search = { search | menu = Topics topicIds Nothing }}
    Closed ->
      logError "onUnhoverItem" "Received \"UnhoverItem\" message when search.menu is Closed"
      model


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
