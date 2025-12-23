port module Feature.TextAPI exposing (viewInput, viewTextarea, enterEdit, leaveEdit, isEdit,
  markdown, openImageFilePicker, update, sub)

import Box
import Box.Size as Size -- TODO: don't import, let caller do the sizing instead
import Feature.Text as Text exposing (EditState(..))
import Item
import Model exposing (Model, Msg(..))
import ModelParts exposing (..)
import Storage as S
import Task
import Undo exposing (UndoModel)
import Utils as U

import Browser.Dom as Dom
import Dict
import Html exposing (Html, input, textarea, text)
import Html.Attributes exposing (id, value)
import Html.Events exposing (onInput)
import Markdown.Block as Block exposing (Block)
import Markdown.Parser as Parser
import Markdown.Renderer as Renderer
import String exposing (fromInt)



-- PORTS


port imageFilePicker : (Id, ImageId) -> Cmd msg

port onPickImageFile : ((Id, ImageId) -> msg) -> Sub msg



-- SUBSCRIPTIONS


sub : Sub Msg
sub =
  onPickImageFile (Text << Text.ImageFilePicked)



-- VIEW


viewInput : TopicInfo -> BoxPath -> Attributes Msg -> Html Msg
viewInput topic boxPath style =
  input
    ( [ id <| Box.elemId "input" topic.id boxPath
      , value topic.text
      , onInput (Text << Text.OnTextInput)
      , U.onEnterOrEsc (Text Text.LeaveEdit)
      , U.onMouseDownStop NoOp -- Prevent drag initiation
      ]
      ++ style
    )
    []


viewTextarea : TopicInfo -> BoxPath -> Attributes Msg -> Html Msg
viewTextarea topic boxPath style =
  textarea
    ( [ id <| Box.elemId "input" topic.id boxPath
      , value topic.text
      , onInput (Text << Text.OnTextareaInput)
      , U.onEsc (Text Text.LeaveEdit)
      , U.onMouseDownStop NoOp -- Prevent drag initiation
      ]
      ++ style
    )
    []



-- UPDATE


update : Text.Msg -> UndoModel -> (UndoModel, Cmd Msg)
update msg ({present} as undoModel) =
  case msg of
    Text.OnTextInput text -> onTextInput text present |> S.store
      |> Undo.swap undoModel
    Text.OnTextareaInput text -> onTextareaInput text present |> S.storeWith
      |> Undo.swap undoModel
    Text.GotTextSize topicId sizeField size ->
      present
        |> Item.setTopicSize topicId sizeField size
        |> Size.auto
        |> S.store
        |> Undo.swap undoModel
    Text.LeaveEdit -> leaveEdit present |> Undo.swap undoModel
    Text.ImageFilePicked (topicId, imageId) -> insertImage topicId imageId present
      |> Undo.swap undoModel


enterEdit : Id -> BoxPath -> Model -> (Model, Cmd Msg)
enterEdit topicId boxPath model =
  let
    newModel =
      model
      |> setEditState (Edit topicId boxPath)
      |> switchTopicDisplay topicId (Box.firstId boxPath)
      |> Size.auto
  in
  (newModel, focus newModel)


leaveEdit : Model -> (Model, Cmd Msg)
leaveEdit model =
  case model.text.edit of
    Edit topicId boxPath ->
      let
        elemId = Box.elemId "topic" topicId boxPath
      in
      ( model
        |> setEditState NoEdit
        |> Size.auto
      , measureElement elemId topicId View
      )
    NoEdit -> (model, Cmd.none)


switchTopicDisplay : Id -> BoxId -> Model -> Model
switchTopicDisplay topicId boxId model =
  model
  |> Box.updateDisplayMode topicId boxId
    (\displayMode ->
      case displayMode of
        TopicD _ -> TopicD Detail
        _ -> displayMode
    )


onTextInput : String -> Model -> Model
onTextInput text model =
  case model.text.edit of
    Edit topicId _ ->
      model
        |> setTopicText topicId text
    NoEdit -> U.logError "onTextInput" "called when text.edit is NoEdit" model


onTextareaInput : String -> Model -> (Model, Cmd Msg)
onTextareaInput text model =
  case model.text.edit of
    Edit topicId _ ->
      model
        |> setTopicText topicId text
        |> measureText topicId text
    NoEdit -> U.logError "onTextareaInput" "called when text.edit is NoEdit" (model, Cmd.none)


measureText : Id -> String -> Model -> (Model, Cmd Msg)
measureText topicId text model =
  ( model |> setMeasureText text
  , measureElement "measure" topicId Editor
  )


measureElement : String -> Id -> SizeField -> Cmd Msg
measureElement elemId topicId sizeField =
  Dom.getElement elemId |> Task.attempt
    (\result ->
      case result of
        Ok {element} -> Text
          (Text.GotTextSize topicId sizeField
            <| Size (round element.width) (round element.height)
          )
        Err err -> U.logError "measureElement" (U.toString err) NoOp
    )


focus : Model -> Cmd Msg
focus model =
  let
    elemId =
      case model.text.edit of
        Edit id boxPath -> Box.elemId "input" id boxPath
        NoEdit -> U.logError "focus" "called when text.edit is NoEdit" ""
  in
  Dom.focus elemId |> Task.attempt
    (\result ->
      case result of
        Ok () -> NoOp
        Err e -> U.logError "focus" (U.toString e) NoOp
    )


isEdit : Id -> BoxPath -> Model -> Bool
isEdit topicId boxPath model =
  model.text.edit == Edit topicId boxPath


setTopicText : Id -> String -> Model -> Model
setTopicText topicId text model =
  model |> Item.updateTopic topicId
    (\topic -> { topic | text = text })


setEditState : EditState -> Model -> Model
setEditState state ({text} as model) =
  { model | text = { text | edit = state }}


setMeasureText : String -> Model -> Model
setMeasureText text_ ({text} as model) =
  { model | text = { text | measure = text_ }}



-- MARKDOWN


markdown : String -> Model -> List (Html Msg)
markdown source model =
  source
    |> Parser.parse
    |> Result.withDefault []
    |> resolveImageUrls model
    |> Renderer.render Renderer.defaultHtmlRenderer
    |> Result.withDefault [ text "Markdown Problem!" ]


openImageFilePicker : Id -> Model -> (Model, Cmd Msg)
openImageFilePicker topicId model =
  let
    imageId = model.nextId
  in
  ( model |> Item.nextId
  , imageFilePicker (topicId, imageId)
  )


insertImage : Id -> ImageId -> Model -> (Model, Cmd Msg)
insertImage topicId imageId model =
  case Item.topicById topicId model of
    Just { text } ->
      let
        image = "![image](app://image/" ++ fromInt imageId ++ ")"
        newText = text ++ image
      in
      model
        |> setTopicText topicId newText
        |> measureText topicId newText
    Nothing -> (model, Cmd.none)


resolveImageUrls : Model -> List Block -> List Block
resolveImageUrls model blocks =
  blocks |> List.map
    ( Block.walkInlines
      (\inline ->
        case inline of
          Block.Image url title altInlines ->
            resolveImageUrl url title altInlines model
          _ -> inline
      )
    )


resolveImageUrl : String -> Maybe String -> List Block.Inline -> Model -> Block.Inline
resolveImageUrl url title altInlines model =
  let
    newUrl =
      case imageIdFromUrl url of
        Just imageId ->
          case model.imageCache |> Dict.get imageId of
            Just blobUrl -> blobUrl
            Nothing ->
              let
                _ = U.info "resolveImageUrl" ("MISSING", imageId)
              in
              url
        Nothing ->
          let
            _ = U.info "resolveImageUrl" ("INVALID", url)
          in
          url
  in
  Block.Image newUrl title altInlines


imageIdFromUrl : String -> Maybe ImageId
imageIdFromUrl url =
  url
    |> String.split "/"
    |> List.reverse
    |> List.head
    |> Maybe.andThen String.toInt
