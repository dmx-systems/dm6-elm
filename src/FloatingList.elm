module FloatingList exposing (viewSearchInput, viewFloatingList, updateSearch)

import Config exposing (..)
import Model exposing (..)

import Dict
import Html exposing (Html, Attribute, div, text, input)
import Html.Attributes exposing (value, style)
import Html.Events exposing (onInput)
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
    SearchResult topicIds ->
      [ div
        floatingListStyle
        ( topicIds |> List.map
          (\id ->
            case getTopicInfo id model of
              Just topic -> div listItemStyle [ text topic.text ]
              Nothing -> text "??"
          )
        )
      ]
    NoList -> []


floatingListStyle : List (Attribute Msg)
floatingListStyle =
  [ style "position" "absolute"
  , style "top" "145px"
  , style "width" "300px"
  , style "padding" "8px"
  , style "font-size" <| fromInt contentFontSize ++ "px"
  , style "line-height" "1.8"
  , style "white-space" "nowrap"
  , style "background-color" "white"
  , style "border" "1px solid lightgray"
  , style "z-index" "2"
  ]


listItemStyle : List (Attribute Msg)
listItemStyle =
  [ style "overflow" "hidden"
  , style "text-overflow" "ellipsis"
  ]



-- UPDATE


updateSearch : SearchMsg -> Model -> (Model, Cmd Msg)
updateSearch msg model =
  case msg of
    SearchInput text -> onSearchInput text model


onSearchInput : String -> Model -> (Model, Cmd Msg)
onSearchInput text model =
  ( { model | searchText = text }
    |> search
  , Cmd.none)


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
  }


isMatch : String -> String -> Bool
isMatch searchText text =
  String.contains searchText text
