port module Feature.Text exposing (viewInput, viewTextarea, enterEdit, leaveEdit, isEdit,
  markdown, openImageFilePicker, update, sub)

import Box
import Config as C
import Console
import Env exposing (Env)
import Feature.TextDef as TextDef exposing (EditState(..), TopicImage)
import Model exposing (Model, Msg(..))
import ModelBase exposing (..)
import Outcome exposing (..)
import Shared.Events as Events
import Task
import Topic

import Browser.Dom as Dom
import Dict
import Html exposing (Html, input, textarea, text)
import Html.Attributes exposing (id, value, placeholder)
import Html.Events exposing (onInput)
import Json.Decode as D
import Json.Encode as E
import Markdown.Block as Block exposing (Block)
import Markdown.Parser as Parser
import Markdown.Renderer as Renderer
import String exposing (fromInt)



-- PORTS


port imageFilePicker : E.Value -> Cmd msg -- value is {topicId, imageId} (TopicImage)

port onImageFilePicked : (E.Value -> msg) -> Sub msg -- value is {topicId, imageId} (TopicImage)

port onImageUrlResolved : ((ImageId, String) -> msg) -> Sub msg



-- SUBSCRIPTIONS


sub : Sub Msg
sub =
  Sub.batch
    [ onImageFilePicked decodeTopicImage
    , onImageUrlResolved (Text << TextDef.ImageUrlResolved)
    ]


decodeTopicImage : E.Value -> Msg
decodeTopicImage value =
  case D.decodeValue topicImageDecoder value of
    Ok topicImage ->
      (Text << TextDef.ImageFilePicked) topicImage
    Err e ->
      let
        _ = Console.logError "Feature.Text.decodeTopicImage" "decoding onImageFilePicked" e
      in
      NoOp


topicImageDecoder : D.Decoder TopicImage
topicImageDecoder =
  D.map2 TopicImage
    (D.field "topicId" topicIdDecoder)
    (D.field "imageId" D.int)



-- VIEW


viewInput : Topic -> BoxPath -> Attrs Msg -> Html Msg
viewInput topic boxPath style =
  input
    ( [ id (Box.elemId "input" topic.id boxPath)
      , value topic.text
      , placeholder C.initTopicText
      , onInput (Text << TextDef.OnTextInput)
      , Events.onEnterOrEsc (Text TextDef.LeaveEdit)
      , Events.onPointerDownStop NoOp -- Prevent drag initiation
      ]
      ++ style
    )
    []


viewTextarea : Topic -> BoxPath -> Attrs Msg -> Html Msg
viewTextarea topic boxPath style =
  textarea
    ( [ id (Box.elemId "input" topic.id boxPath)
      , value topic.text
      , placeholder C.initTopicText
      , onInput (Text << TextDef.OnTextareaInput)
      , Events.onEsc (Text TextDef.LeaveEdit)
      , Events.onPointerDownStop NoOp -- Prevent drag initiation
      ]
      ++ style
    )
    []



-- UPDATE


update : TextDef.Msg -> Env -> Outcome
update msg ({model} as env) =
  case msg of
    TextDef.OnTextInput text ->
      model
        |> onTextInput text
        |> Outcome.from (Directives Store Swap)
    TextDef.OnTextareaInput text ->
      model
        |> onTextareaInput text
        |> Outcome.newWith (Directives Store Swap)
    TextDef.GotTextSize topicId sizeField size ->
      env
        |> Env.map (Topic.setSize topicId sizeField size)
        |> Env.autoSize
        |> Env.outcomeWith (Directives Store Swap)
    TextDef.LeaveEdit ->
      env
        |> leaveEdit
        |> Outcome.new
    TextDef.ImageFilePicked {topicId, imageId} ->
      model
        |> insertImage topicId imageId
        |> Outcome.new
    TextDef.ImageUrlResolved (imageId, url) ->
      model
        |> addToImageCache imageId url
        |> Outcome.default


enterEdit : TopicId -> BoxPath -> Env -> (Model, Cmd Msg)
enterEdit topicId boxPath env =
  let
    model =
      env
        |> Env.map (setEditState (Edit topicId boxPath))
        |> Env.map (setExpansion topicId (Box.firstId boxPath))
        |> Env.autoSize
        |> .model
  in
  (model, focus model)


leaveEdit : Env -> (Model, Cmd Msg)
leaveEdit ({model} as env) =
  case model.text.edit of
    Edit topicId boxPath ->
      let
        elemId = Box.elemId "topic" topicId boxPath
      in
      ( env
          |> Env.map (setEditState NoEdit)
          |> Env.autoSize
          |> .model
      , measureElement elemId topicId View
      )
    NoEdit ->
      (model, Cmd.none)


setExpansion : TopicId -> BoxId -> Model -> Model
setExpansion topicId boxId model =
  model
    |> Box.updateExpansion topicId boxId
      (\expansion ->
        if Topic.isBox topicId model then
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
    NoEdit ->
      let
        _ = Console.logError "Feature.Text.onTextInput" "Unexpected model.text.edit state"
          model.text.edit
      in
      model


onTextareaInput : String -> Model -> (Model, Cmd Msg)
onTextareaInput text model =
  case model.text.edit of
    Edit topicId _ ->
      model
        |> setTopicText topicId text
        |> measureText topicId text
    NoEdit ->
      let
        _ = Console.logError "Feature.Text.onTextareaInput" "Unexpected model.text.edit state"
          model.text.edit
      in
      (model, Cmd.none)


measureText : TopicId -> String -> Model -> (Model, Cmd Msg)
measureText topicId text model =
  ( model
      |> setMeasureText text
  , measureElement "measure" topicId Editor
  )


measureElement : String -> TopicId -> SizeField -> Cmd Msg
measureElement elemId topicId sizeField =
  Dom.getElement elemId |> Task.attempt
    (\result ->
      case result of
        Ok {element} -> Text
          (TextDef.GotTextSize topicId sizeField
            <| Size (round element.width) (round element.height)
          )
        -- Note: not all renderers do text measuring (the TopicList renderer does not).
        -- TODO: don't call in the first place and regard dom-not-found an error.
        Err err ->
          {- Console.logError "Feature.Text.measureElement" (Console.toString err) -}
          NoOp
    )


focus : Model -> Cmd Msg
focus model =
  let
    elemId : String
    elemId =
      case model.text.edit of
        Edit id boxPath ->
          Box.elemId "input" id boxPath
        NoEdit ->
          let
            _ = Console.logError "Feature.Text.focus" "Unexpected model.text.edit state"
              model.text.edit
          in
          ""
  in
  Dom.focus elemId
    |> Task.attempt
      (\result ->
        case result of
          Ok () -> NoOp
          Err e -> Console.logError "Feature.Text.focus" (Console.toString e) NoOp
      )


isEdit : TopicId -> BoxPath -> Model -> Bool
isEdit topicId boxPath model =
  model.text.edit == Edit topicId boxPath


setTopicText : TopicId -> String -> Model -> Model
setTopicText topicId text model =
  model |> Topic.update topicId
    (\topic -> { topic | text = text })


setEditState : EditState -> Model -> Model
setEditState state ({text} as model) =
  { model | text = { text | edit = state }}


setMeasureText : String -> Model -> Model
setMeasureText text_ ({text} as model) =
  { model | text = { text | measure = text_ }}


addToImageCache : ImageId -> String -> Model -> Model
addToImageCache imageId url ({text} as model) =
  { model | text = { text | imageCache = text.imageCache |> Dict.insert imageId url }}



-- MARKDOWN


markdown : String -> Model -> List (Html Msg)
markdown source model =
  source
    |> Parser.parse
    |> Result.withDefault []
    |> resolveImageUrls model
    |> Renderer.render Renderer.defaultHtmlRenderer
    |> Result.withDefault [ text "Markdown Problem!" ]


openImageFilePicker : TopicId -> Model -> (Model, Cmd Msg)
openImageFilePicker (TopicId topicId) model =
  let
    imageId = model.nextId
  in
  ( model
      |> Model.nextId
  , imageFilePicker
      ( E.object
          [ ("topicId", E.int topicId)
          , ("imageId", E.int imageId)
          ]
      )
  )


insertImage : TopicId -> ImageId -> Model -> (Model, Cmd Msg)
insertImage topicId imageId model =
  case Topic.fromId topicId model of
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
          case model.text.imageCache |> Dict.get imageId of
            Just blobUrl -> blobUrl
            Nothing ->
              let
                _ = Console.info "Feature.Text.resolveImageUrl" ("MISSING", imageId)
              in
              url
        Nothing ->
          let
            _ = Console.info "Feature.Text.resolveImageUrl" ("INVALID", url)
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
