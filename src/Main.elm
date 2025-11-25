module Main exposing (..)

import Box
import Box.Size as Size
import Config as C
import Item
import Map
import Model exposing (Model, Msg(..))
import ModelHelper exposing (..)
import Storage as S
import Undo exposing (UndoModel)
import Utils as U
-- feature modules
import IconAPI
import MouseAPI
import Nav
import NavAPI
import SearchAPI
import SelectionAPI as Sel
import TextEditAPI
import ToolAPI

import Browser
import Browser.Navigation exposing (Key)
import Html exposing (Html, Attribute, div, text, br, a)
import Html.Attributes exposing (id, style, href)
import Json.Decode as D
import Json.Encode as E
import String exposing (fromInt, fromFloat)
import Url exposing (Url)



-- MAIN


main : Program E.Value UndoModel Msg
main =
  Browser.application
    { init = init
    , view = view
    , update = update
    , subscriptions = MouseAPI.subs
    , onUrlChange = Nav << Nav.UrlChanged
    , onUrlRequest = Nav << Nav.LinkClicked
    }


init : E.Value -> Url -> Key -> (UndoModel, Cmd Msg)
init flags url key =
  (initModel flags key, Cmd.none) |> Undo.reset


initModel : E.Value -> Key -> Model
initModel flags key =
  case flags |> D.decodeValue (D.null True) of
    Ok True ->
      let
        _ = U.info "init" "localStorage: empty"
      in
      Model.init key
    _ ->
      case flags |> D.decodeValue (Model.decoder key) of
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
          Model.init key



-- VIEW


view : UndoModel -> Browser.Document Msg
view ({present} as undoModel) =
  Browser.Document
    "DM6 Elm"
    [ div
      appStyle
      [ ToolAPI.viewAppHeader undoModel
      , div
        ( mainStyle
          ++ MouseAPI.hoverHandler
        )
        ( [ Map.view (Box.active present) [] present ] -- boxPath = []
          ++ SearchAPI.viewMenu present
          ++ IconAPI.viewMenu present
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


mainStyle : List (Attribute Msg)
mainStyle =
  [ style "position" "relative"
  , style "flex-grow" "1"
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
  [ style "font-size" <| fromInt C.footerFontSize ++ "px"
  , style "position" "absolute"
  , style "bottom" "0"
  , style "color" "lightgray"
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
  , style "font-family" C.mainFont
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
    MoveTopicToBox topicId boxId origPos targetId targetBoxPath pos
      -> moveTopicToBox topicId boxId origPos targetId targetBoxPath pos present |> S.store
      |> Undo.push undoModel
    DraggedTopic -> present |> S.store |> Undo.swap undoModel
    ClickedItem itemId boxPath -> select itemId boxPath present |> Undo.swap undoModel
    ClickedBackground -> resetUI present |> Undo.swap undoModel
    -- feature modules
    Tool toolMsg -> ToolAPI.update toolMsg undoModel
    Edit editMsg -> TextEditAPI.update editMsg undoModel
    Mouse mouseMsg -> MouseAPI.update mouseMsg undoModel
    Search searchMsg -> SearchAPI.update searchMsg undoModel
    Icon iconMenuMsg -> IconAPI.update iconMenuMsg undoModel
    Nav navMsg -> NavAPI.update navMsg present |> S.store |> Undo.reset
    --
    NoOp -> (present, Cmd.none) |> Undo.swap undoModel


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
moveTopicToBox topicId boxId origPos targetId targetBoxPath pos model =
  let
    props_ =
      Box.topicProps topicId boxId model
      |> Maybe.andThen (\props -> Just (TopicV { props | pos = pos }))
  in
  case props_ of
    Just props ->
      model
      |> Box.hideItem topicId boxId
      |> Box.setTopicPos topicId boxId origPos
      |> Box.addItem topicId props targetId
      |> Sel.select targetId targetBoxPath
      |> Size.auto
    Nothing -> model


select : Id -> BoxPath -> Model -> (Model, Cmd Msg)
select itemId boxPath model =
  ( model
    |> Sel.select itemId boxPath
  , Cmd.none
  )


resetUI : Model -> (Model, Cmd Msg)
resetUI model =
  ( model
    |> Sel.clear
    |> IconAPI.closeMenu
    |> SearchAPI.closeMenu
  , Cmd.none
  )
