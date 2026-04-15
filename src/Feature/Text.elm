port module Feature.Text exposing (viewInput, viewTextarea, enterEdit, leaveEdit, isEdit,
  markdown, openImageFilePicker, update, sub)

import Box
import Config as C
import Env exposing (Env)
import Feature.TextDef as TextDef exposing (EditState(..))
import Item
import Model exposing (Model, Msg(..))
import ModelBase exposing (..)
import Storage as S
import Task
import Undo exposing (UndoModel)
import Utils as U

import Browser.Dom as Dom
import Dict
import Html exposing (Html, input, textarea, text)
import Html.Attributes exposing (id, value, placeholder)
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
  onPickImageFile (Text << TextDef.ImageFilePicked)



-- VIEW


viewInput : TopicInfo -> BoxPath -> Attrs Msg -> Html Msg
viewInput topic boxPath style =
  input
    ( [ id <| Box.elemId "input" topic.id boxPath
      , value topic.text
      , placeholder C.initBoxText
      , onInput (Text << TextDef.OnTextInput)
      , U.onEnterOrEsc (Text TextDef.LeaveEdit)
      , U.onMouseDownStop NoOp -- Prevent drag initiation
      ]
      ++ style
    )
    []


viewTextarea : TopicInfo -> BoxPath -> Attrs Msg -> Html Msg
viewTextarea topic boxPath style =
  textarea
    ( [ id <| Box.elemId "input" topic.id boxPath
      , value topic.text
      , placeholder C.initTopicText
      , onInput (Text << TextDef.OnTextareaInput)
      , U.onEsc (Text TextDef.LeaveEdit)
      , U.onMouseDownStop NoOp -- Prevent drag initiation
      ]
      ++ style
    )
    []



-- UPDATE


update : TextDef.Msg -> Env -> (UndoModel, Cmd Msg)
update msg ({model, undoModel, ext} as env) =
  case msg of
    TextDef.OnTextInput text -> onTextInput text model |> S.store
      |> Undo.swap undoModel
    TextDef.OnTextareaInput text -> onTextareaInput text model |> S.storeWith
      |> Undo.swap undoModel
    TextDef.GotTextSize topicId sizeField size ->
      model
        |> Item.setTopicSize topicId sizeField size
        |> Env.autoSize env
        |> S.store
        |> Undo.swap undoModel
    TextDef.LeaveEdit -> leaveEdit env |> Undo.swap undoModel
    TextDef.ImageFilePicked (topicId, imageId) -> insertImage topicId imageId model
      |> Undo.swap undoModel


enterEdit : Id -> BoxPath -> Env -> (Model, Cmd Msg)
enterEdit topicId boxPath ({model} as env) =
  let
    newModel =
      model
        |> setEditState (Edit topicId boxPath)
        |> switchTopicDisplay topicId (Box.firstId boxPath)
        |> Env.autoSize env
  in
  (newModel, focus newModel)


leaveEdit : Env -> (Model, Cmd Msg)
leaveEdit ({model} as env) =
  case model.text.edit of
    Edit topicId boxPath ->
      let
        elemId = Box.elemId "topic" topicId boxPath
      in
      ( model
          |> setEditState NoEdit
          |> Env.autoSize env
      , measureElement elemId topicId View
      )
    NoEdit -> (model, Cmd.none)


switchTopicDisplay : Id -> BoxId -> Model -> Model
switchTopicDisplay topicId boxId model =
  model
    |> Box.updateExpansion topicId boxId
      (\expansion ->
        if Item.isBox topicId model then
          expansion
        else
          Expanded
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
          (TextDef.GotTextSize topicId sizeField
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
