module IconMenu exposing (updateIconMenu, viewIconMenu, viewTopicIcon)

import Model exposing (..)
import Style exposing (..)

import Dict
import Html exposing (Html, div, text, button)
import Html.Attributes exposing (title)
import Html.Events exposing (onClick)
import FeatherIcons as Icon



-- UPDATE


updateIconMenu : IconMenuMsg -> Model -> Model
updateIconMenu msg model =
  case msg of
    Open -> setEditDialogOpen True model
    Close -> setEditDialogOpen False model
    SetIcon maybeIcon -> setIcon maybeIcon model
      |> setEditDialogOpen False


setEditDialogOpen : Bool -> Model -> Model
setEditDialogOpen isOpen model =
  { model | isIconMenuOpen = isOpen }


setIcon : Maybe IconName -> Model -> Model
setIcon iconName model =
  case getSingleSelection model of
    Just (id, _) -> updateTopicInfo id
      (\topic -> { topic | iconName = iconName })
      model
    Nothing -> model -- FIXME: illegal state -> make Edit dialog modal



-- VIEW


viewIconMenu : Model -> Html Msg
viewIconMenu model =
  div
    editDialogStyle
    [ div
        iconsListStyle
        viewIcons
    , button
      ( [onClick (IconMenu Close)]
        ++ closeButtonStyle
      )
      [ Icon.x
        |> Icon.withSize 12
        |> Icon.toHtml []
      ]
    ]


viewIcons : List (Html Msg)
viewIcons =
  Icon.icons |> Dict.toList |> List.map
    (\(iconName, icon) ->
      button
        ( [ onClick (Just iconName |> SetIcon |> IconMenu)
          , stopPropagationOnMousedown
          , title iconName
          ]
          ++ iconButtonStyle
        )
        [ Icon.toHtml [] icon ]
    )


viewTopicIcon : Id -> Model -> Html Msg
viewTopicIcon topicId model =
  case getTopicInfo topicId model of
    Just topic ->
      case topic.iconName of
        Just iconName ->
          case Icon.icons |> Dict.get iconName of
            Just icon -> icon |> Icon.withSize topicIconSize |> Icon.toHtml topicIconStyle
            Nothing -> text "??"
        Nothing -> text ""
    Nothing -> text "?"
