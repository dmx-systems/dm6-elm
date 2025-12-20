port module Main exposing (..)

import Box
import Box.Size as Size
import Config as C
import Feature.IconAPI as IconAPI
import Feature.MouseAPI as MouseAPI
import Feature.NavAPI as NavAPI
import Feature.SearchAPI as SearchAPI
import Feature.SelAPI as SelAPI
import Feature.TextEditAPI as TextEditAPI
import Feature.ToolAPI as ToolAPI
import Item
import Map
import Model exposing (Model, Msg(..))
import ModelParts exposing (..)
import Storage as S
import Undo exposing (UndoModel)
import Utils as U

import Browser
import Dict
import Html exposing (Html, Attribute, div, text, br, a)
import Html.Attributes exposing (id, style, href)
import Json.Decode as D
import Json.Encode as E
import String exposing (fromInt, fromFloat)



-- PORTS


port onScroll : (Point -> msg) -> Sub msg

port onPickFile : ((Id, ImageId) -> msg) -> Sub msg

port onResolveUrl : ((ImageId, String) -> msg) -> Sub msg



-- MAIN


main : Program (E.Value, String) UndoModel Msg
main =
  Browser.document
    { init = init
    , view = view
    , update = update
    , subscriptions =
      (\model -> Sub.batch
        [ MouseAPI.sub model
        , NavAPI.sub
        , onScroll Scrolled
        , onPickFile FilePicked
        , onResolveUrl UrlResolved
        ]
      )
    }


init : (E.Value, String) -> (UndoModel, Cmd Msg)
init (flags, hash) =
  let
    model = initModel flags
    boxId =
      case NavAPI.boxIdFromHash hash of
        Just boxId_ -> boxId_
        Nothing -> model.boxId
    cmd = NavAPI.pushUrl boxId
  in
  (model, cmd) |> Undo.reset


initModel : E.Value -> Model
initModel flags =
  case flags |> D.decodeValue (D.null True) of
    Ok True ->
      let
        _ = U.info "init" "localStorage: empty"
      in
      Model.init
    _ ->
      case flags |> D.decodeValue Model.decoder of
        Ok model ->
          let
            _ = U.info "init" ("localStorage: " ++ bytes ++ " bytes")
            bytes = model |> U.toString |> String.length |> fromInt
          in
          model
        Err e ->
          let
            _ = U.logError "init" "localStorage" e
          in
          Model.init



-- VIEW


view : UndoModel -> Browser.Document Msg
view ({present} as undoModel) =
  Browser.Document
    "DM6 Elm"
    [ div
      appStyle
      [ div
        headerStyle
        ( [ viewMapTitle present
          , viewSpacer
          ]
          ++ ToolAPI.viewGlobalTools present
          ++ SearchAPI.viewSearchResult present -- TODO: move to "main" for scrolling along?
        )
      , div
        ( [ id "main" ]
          ++ mainStyle
        )
        ( [ Map.view present.boxId [] present ] -- boxPath = []
          ++ ToolAPI.viewMapTools undoModel
        )
      ]
    , viewFooter
    , viewMeasure present
    ]


appStyle : List (Attribute Msg)
appStyle =
  [ style "display" "flex"
  , style "flex-direction" "column"
  , style "height" "100%"
  , style "font-family" C.mainFont
  , style "user-select" "none"
  , style "-webkit-user-select" "none" -- Safari still needs vendor prefix
  ]


headerStyle : List (Attribute Msg)
headerStyle =
  [ style "display" "flex"
  , style "align-items" "center"
  , style "gap" "18px"
  , style "height" <| fromInt C.appHeaderHeight ++ "px"
  , style "padding" "0 8px"
  , style "background-color" C.toolbarColor
  ]


viewMapTitle : Model -> Html Msg
viewMapTitle model =
  div
    mapTitleStyle
    [ text <| Box.mapTitle model ]


mapTitleStyle : List (Attribute Msg)
mapTitleStyle =
  [ style "font-size" "24px"
  , style "font-weight" "bold"
  , style "overflow" "hidden"
  , style "text-overflow" "ellipsis"
  , style "white-space" "nowrap"
  ]


viewSpacer : Html Msg
viewSpacer =
  div
  [ style "flex-grow" "1" ]
  []


mainStyle : List (Attribute Msg)
mainStyle =
  [ style "position" "relative"
  , style "flex-grow" "1"
  , style "overflow" "auto"
  ]


viewFooter : Html Msg
viewFooter =
  div
    footerStyle
    [ div
      []
      [ text C.version ]
    , div
      []
      [ text C.date ]
    , div
      []
      [ text "Source: "
      , a
        ( [ href "https://github.com/dmx-systems/dm6-elm" ]
          ++ linkStyle
        )
        [ text "GitHub" ]
      ]
    , a
      ( [ href "https://dmx.berlin" ]
        ++ linkStyle
      )
      [ text "DMX Systems" ]
    ]


footerStyle : List (Attribute Msg)
footerStyle =
  [ style "font-family" C.mainFont
  , style "font-size" <| fromInt C.footerFontSize ++ "px"
  , style "position" "absolute"
  , style "bottom" "20px"
  , style "right" "20px"
  , style "text-align" "right"
  , style "color" "lightgray" -- #d3d3d3
  ]


linkStyle : List (Attribute Msg)
linkStyle =
  [ style "color" "lightgray" ]


viewMeasure : Model -> Html Msg
viewMeasure model =
  div
    ( [ id "measure" ]
      ++ measureStyle
    )
    [ text model.edit.measureText
    , br [] []
    ]


measureStyle : List (Attribute Msg)
measureStyle =
  [ style "position" "fixed"
  , style "visibility" "hidden"
  , style "white-space" "pre-wrap"
  , style "font-family" C.editorFont
  , style "font-size" <| fromInt C.contentFontSize ++ "px"
  , style "line-height" <| fromFloat C.topicLineHeight
  , style "padding" <| fromInt C.topicDetailPadding ++ "px"
  , style "width" <| fromFloat C.topicDetailMaxWidth ++ "px"
  , style "min-width" <| fromFloat (C.topicSize.w - C.topicSize.h) ++ "px"
  , style "max-width" "max-content"
  , style "border-width" <| fromFloat C.topicBorderWidth ++ "px"
  , style "border-style" "solid"
  , style "box-sizing" "border-box"
  ]



-- UPDATE


update : Msg -> UndoModel -> (UndoModel, Cmd Msg)
update msg ({present} as undoModel) =
  let
    _ =
      case msg of
        Mouse _ -> msg
        _ -> U.info "update" msg
  in
  case msg of
    -- gestures detected by Mouse module
    AddAssoc player1 player2 boxId -> addAssoc player1 player2 boxId present |> S.store
      |> Undo.push undoModel
    MoveTopicToBox topicId boxId origPos targetId targetPath pos -> moveTopicToBox topicId boxId
      origPos targetId targetPath pos present |> S.store |> Undo.push undoModel
    TopicDragged -> present |> S.store |> Undo.swap undoModel
    ItemClicked itemId boxPath -> select itemId boxPath present |> Undo.swap undoModel
    Cancel maybeTarget -> cancelUI maybeTarget present |> Undo.swap undoModel
    -- feature modules
    Tool toolMsg -> ToolAPI.update toolMsg undoModel
    Edit editMsg -> TextEditAPI.update editMsg undoModel
    Mouse mouseMsg -> MouseAPI.update mouseMsg undoModel
    Search searchMsg -> SearchAPI.update searchMsg undoModel
    Icon iconMenuMsg -> IconAPI.update iconMenuMsg undoModel
    Nav navMsg -> NavAPI.update navMsg undoModel
    --
    Scrolled pos -> updateScrollPos pos present |> S.store |> Undo.swap undoModel
    FilePicked (topicId, imageId) -> insertImage topicId imageId present |> Undo.swap undoModel
    UrlResolved (imageId, url) -> cacheImageUrl imageId url present |> Undo.swap undoModel
    NoOp -> (undoModel, Cmd.none)


-- Presumption: both players exist in same box
addAssoc : Id -> Id -> BoxId -> Model -> Model
addAssoc player1 player2 boxId model =
  addAssocAndAddToBox
    "dmx.association"
    "dmx.default" player1
    "dmx.default" player2
    boxId model


-- Presumption: both players exist in same box
addAssocAndAddToBox : ItemType -> RoleType -> Id -> RoleType -> Id -> BoxId -> Model -> Model
addAssocAndAddToBox itemType role1 player1 role2 player2 boxId model =
  let
    (newModel, assocId) = Item.addAssoc itemType role1 player1 role2 player2 model
    props = AssocV AssocProps
  in
  Box.addItem assocId props boxId newModel


moveTopicToBox : Id -> BoxId -> Point -> Id -> BoxPath -> Point -> Model -> Model
moveTopicToBox topicId boxId origPos targetId targetPath pos model =
  let
    props_ =
      Box.topicProps topicId boxId model
      |> Maybe.andThen (\props -> Just (TopicV { props | pos = pos }))
  in
  case props_ of
    Just props ->
      model
      |> Box.removeItem topicId boxId
      |> Box.setTopicPos topicId boxId origPos
      |> Box.addItem topicId props targetId
      |> SelAPI.select targetId targetPath
      |> Size.auto
    Nothing -> model


select : Id -> BoxPath -> Model -> (Model, Cmd Msg)
select itemId boxPath model =
  ( model
    |> SelAPI.select itemId boxPath
  , Cmd.none
  )


cancelUI : Maybe (Id, BoxPath) -> Model -> (Model, Cmd Msg)
cancelUI maybeTarget model =
  let
    shouldClear =
      case maybeTarget of
        Just (itemId, boxPath) -> not <| SelAPI.isSelected itemId (Box.firstId boxPath) model
        Nothing -> True
  in
  ( model
    |> (if shouldClear then SelAPI.clear else identity)
    |> IconAPI.closePicker
    |> SearchAPI.closeMenu
  , Cmd.none
  )


updateScrollPos : Point -> Model -> Model
updateScrollPos pos model =
  Box.updateScrollPos model.boxId (\_ -> pos) model


insertImage : Id -> ImageId -> Model -> (Model, Cmd Msg)
insertImage topicId imageId model =
  let
    markdown = "![image](app://image/" ++ fromInt imageId ++ ")"
  in
  ( model |> Item.updateTopic topicId
    (\topic -> { topic | text = topic.text ++ markdown })
  , Cmd.none
  )


cacheImageUrl : ImageId -> String -> Model -> (Model, Cmd Msg)
cacheImageUrl imageId url model =
  ( { model | imageCache = model.imageCache |> Dict.insert imageId url }
  , Cmd.none
  )
