module EditDialog exposing (updateEditDialog, viewEditDialog, viewTopicIcon)

import Model exposing (..)
import Style exposing (..)

import Dict
import Html exposing (Html, div, text, button)
import Html.Attributes exposing (title)
import Html.Events exposing (onClick)
import FeatherIcons as Icon



-- UPDATE


updateEditDialog : EditMsg -> Model -> Model
updateEditDialog msg model =
  case msg of
    ItemEditStart -> startItemEdit model
    ItemEditInput text -> updateItemText text model
    ItemEditEnd -> endItemEdit model
    Open -> setEditDialogOpen True model
    Close -> setEditDialogOpen False model
    SetIcon maybeIcon -> setIcon maybeIcon model
      |> setEditDialogOpen False


setEditDialogOpen : Bool -> Model -> Model
setEditDialogOpen isOpen model =
  { model | isEditDialogOpen = isOpen }


startItemEdit : Model -> Model
startItemEdit model =
  case getSingleSelection model of
    Just (id, _) -> { model | editState = ItemEdit id }
    Nothing -> model


updateItemText : String -> Model -> Model
updateItemText text model =
  case model.editState of
    ItemEdit id -> updateTopicInfo id
      (\topic -> { topic | text = text })
      model
    NoEdit -> logError "updateItemText" "called when editState is NoEdit" model


endItemEdit : Model -> Model
endItemEdit model =
  { model | editState = NoEdit }


setIcon : Maybe IconName -> Model -> Model
setIcon iconName model =
  case getSingleSelection model of
    Just (id, _) -> updateTopicInfo id
      (\topic -> { topic | iconName = iconName })
      model
    Nothing -> model -- FIXME: illegal state -> make Edit dialog modal



-- VIEW


viewEditDialog : Model -> Html Msg
viewEditDialog model =
  div
    editDialogStyle
    [ div
        iconsListStyle
        viewIcons
    , button
      ( [onClick (Edit Close)]
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
        ( [ onClick (Just iconName |> SetIcon |> Edit)
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
