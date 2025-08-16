module FloatingList exposing (viewSearchInput, viewFloatingList, updateSearch)

import Config exposing (..)
import Model exposing (..)
import Utils exposing (..)

import Dict
import Html exposing (Html, Attribute, div, text, input)
import Html.Attributes exposing (value, style, attribute)
import Html.Events exposing (onInput, on)
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
      ( [ value model.searchText
        , onInput (Search << SearchInput)
        ]
        ++searchInputStyle
      )
      []
    ]


searchInputStyle : List (Attribute Msg)
searchInputStyle =
  [ style "width" "100px" ]


viewFloatingList : Model -> List (Html Msg)
viewFloatingList model =
  case model.listState of
    SearchResult topicIds maybeId ->
      [ div
        ( [ on "mouseover" (hoverDecoder OverItem)
          , on "mouseout" (hoverDecoder OutItem)
          ]
          ++ floatingListStyle
        )
        ( topicIds |> List.map
          (\id ->
            case getTopicInfo id model of
              Just topic ->
                div
                  ( [ attribute "data-id" (fromInt topic.id) ]
                    ++ listItemStyle topic.id model
                  )
                  [ text topic.text ]
              Nothing -> text "??"
          )
        )
      ]
    NoList -> []


hoverDecoder : (Id -> SearchMsg) -> D.Decoder Msg
hoverDecoder msg =
  D.map Search <| D.map msg
    (D.at ["target", "dataset", "id"] D.string |> D.andThen strToIntDecoder)


floatingListStyle : List (Attribute Msg)
floatingListStyle =
  [ style "position" "absolute"
  , style "top" "145px"
  , style "width" "240px"
  , style "padding" "3px 0"
  , style "font-size" <| fromInt contentFontSize ++ "px"
  , style "line-height" "2"
  , style "white-space" "nowrap"
  , style "background-color" "white"
  , style "border" "1px solid lightgray"
  , style "z-index" "2"
  ]


listItemStyle : Id -> Model -> List (Attribute Msg)
listItemStyle topicId model =
  let
    isHover = case model.listState of
      SearchResult _ maybeId -> maybeId == Just topicId
      NoList -> False
  in
  [ style "color" (if isHover then "white" else "black")
  , style "background-color" (if isHover then "black" else "white")
  , style "overflow" "hidden"
  , style "text-overflow" "ellipsis"
  , style "padding" "0 8px"
  ]



-- UPDATE


updateSearch : SearchMsg -> Model -> (Model, Cmd Msg)
updateSearch msg model =
  case msg of
    SearchInput text -> onSearchInput text model
    OverItem topicId ->
      case model.listState of
        SearchResult topicIds _ ->
          ( { model | listState = SearchResult topicIds (Just topicId) } -- update hovered topic
          , Cmd.none
          )
        NoList ->
          logError "updateSearch" "Received \"OverItem\" message when listState is NoList"
          (model, Cmd.none)
    OutItem topicId ->
      case model.listState of
        SearchResult topicIds _ ->
          ( { model | listState = SearchResult topicIds Nothing } -- update hovered topic
          , Cmd.none
          )
        NoList ->
          logError "updateSearch" "Received \"OutItem\" message when listState is NoList"
          (model, Cmd.none)


onSearchInput : String -> Model -> (Model, Cmd Msg)
onSearchInput text model =
  ( { model | searchText = text }
    |> search
  , Cmd.none
  )


search : Model -> Model
search model =
  { model | listState = SearchResult
    ( model.items |> Dict.foldr
        (\id item topicIds ->
          case item of
            Topic {text} ->
              if isMatch model.searchText text then
                id :: topicIds
              else
                topicIds
            Assoc _ -> topicIds
        )
        []
    )
    Nothing
  }


isMatch : String -> String -> Bool
isMatch searchText text =
  String.contains searchText text
