port module Main exposing (..)

import Box
import Config as C
import Env exposing (Env)
import ExtManager
import Feature.Icon as Icon
import Feature.MouseDef as MouseDef
import Feature.Mouse as Mouse
import Feature.Nav as Nav
import Feature.Search as Search
import Feature.Sel as Sel
import Feature.Text as Text
import Feature.Tool as Tool
import Item
import Model exposing (Model, Msg(..))
import ModelBase exposing (..)
import Storage as S
import TopicMap.TopicMap as TM
import TopicMap.TopicMapDef exposing (ItemProps(..), AssocProps)
import Undo exposing (UndoModel)
import Utils as U

import Browser
import Dict
import Html exposing (Html, div, text, br, a)
import Html.Attributes exposing (id, style, href)
import Json.Decode as D
import Json.Encode as E
import String exposing (fromInt, fromFloat)



-- PORTS


port onScroll : (Point -> msg) -> Sub msg

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
          [ Text.sub
          , Nav.sub
          , onScroll Scrolled
          , onResolveUrl UrlResolved
          ]
        )
    }


init : (E.Value, String) -> (UndoModel, Cmd Msg)
init (flags, hash) =
  let
    model = initModel flags
    boxId =
      case Nav.boxIdFromHash hash of
        Just boxId_ -> boxId_
        Nothing -> model.boxId
    cmd = Nav.pushUrl boxId
  in
  (model, cmd) |> Undo.reset


initModel : E.Value -> Model
initModel flags =
  case flags |> D.decodeValue (D.null True) of
    Ok True ->
      let
        _ = U.info "Main.initModel" "localStorage: empty"
      in
      Model.init
    _ ->
      case flags |> D.decodeValue Model.decoder of
        Ok model ->
          let
            _ = U.info "Main.initModel" ("localStorage: " ++ bytes ++ " bytes")
            bytes = model |> U.toString |> String.length |> fromInt
          in
          model
        Err e ->
          let
            _ = U.logError "Main.initModel" "localStorage" e
          in
          Model.init



-- VIEW


view : UndoModel -> Browser.Document Msg
view ({present} as undoModel) =
  Browser.Document
    "DM6 Elm"
    [ div
        ( Mouse.dragHandler
          ++ appStyle
        )
        [ div
            headerStyle
            ( [ viewMapTitle present
              , viewSpacer
              ]
              ++ Tool.viewGlobalTools present
              ++ Search.viewSearchResult present -- TODO: move to "main" for scroll along?
            )
        , div
            ( [ id "main" ]
              ++ mainStyle
            )
            ( [ ExtManager.ext.view present.boxId [] present ] -- boxPath = []
              ++ Tool.viewMapTools undoModel
            )
        ]
    , viewFooter
    , viewMeasure present
    ]


appStyle : Attrs Msg
appStyle =
  [ style "display" "flex"
  , style "flex-direction" "column"
  , style "height" "100%"
  , style "font-family" C.mainFont
  , style "user-select" "none"
  , style "-webkit-user-select" "none" -- Safari still needs vendor prefix
  ]


headerStyle : Attrs Msg
headerStyle =
  [ style "display" "flex"
  , style "align-items" "center"
  , style "gap" "10px"
  , style "height" <| fromInt C.appHeaderHeight ++ "px"
  , style "padding" "0 10px"
  , style "background-color" C.toolbarColor
  ]


viewMapTitle : Model -> Html Msg
viewMapTitle model =
  div
    mapTitleStyle
    [ text <| Box.mapTitle model ]


mapTitleStyle : Attrs Msg
mapTitleStyle =
  [ style "font-size" "26px"
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


mainStyle : Attrs Msg
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
        [ text "DMX Berlin" ]
    ]


footerStyle : Attrs Msg
footerStyle =
  [ style "font-family" C.mainFont
  , style "font-size" <| fromInt C.footerFontSize ++ "px"
  , style "position" "absolute"
  , style "bottom" "20px"
  , style "right" "20px"
  , style "text-align" "right"
  , style "color" "lightgray" -- #d3d3d3
  ]


linkStyle : Attrs Msg
linkStyle =
  [ style "color" "lightgray" ]


viewMeasure : Model -> Html Msg
viewMeasure model =
  div
    ( [ id "measure" ]
      ++ measureStyle
    )
    [ text model.text.measure
    , br [] []
    ]


measureStyle : Attrs Msg
measureStyle =
  [ style "position" "fixed"
  , style "visibility" "hidden"
  , style "white-space" "pre-wrap"
  , style "font-family" C.editorFont
  , style "font-size" <| fromInt C.contentFontSize ++ "px"
  , style "line-height" <| fromFloat C.topicLineHeight
  , style "padding" <| fromInt C.topicDetailPadding ++ "px"
  , style "width" <| fromInt C.topicDetailMaxWidth ++ "px"
  , style "min-width" <| fromInt (C.topicSize.w - C.topicSize.h) ++ "px"
  , style "max-width" "max-content"
  , style "border-width" <| fromInt C.topicBorderWidth ++ "px"
  , style "border-style" "solid"
  , style "box-sizing" "border-box"
  ]



-- UPDATE


update : Msg -> UndoModel -> (UndoModel, Cmd Msg)
update msg ({present} as undoModel) =
  let
    env =
      { model = present
      , undoModel = undoModel
      , ext = ExtManager.ext
      }
    _ =
      case msg of
        Mouse (MouseDef.Move _) -> msg
        _ -> U.info "Main.update" msg
  in
  case msg of
    -- gestures detected by Mouse module
    CreateAssoc player1 player2 boxId -> createAssoc player1 player2 boxId present |> S.store
      |> Undo.push undoModel
    MoveTopicToBox topicId boxId origPos targetId targetPath pos -> moveTopicToBox topicId boxId
      origPos targetId targetPath pos env |> S.store |> Undo.push undoModel
    TopicDragged -> present |> S.store |> Undo.swap undoModel
    ItemClicked itemId boxPath -> select itemId boxPath present |> Undo.swap undoModel
    Cancel maybeTarget -> cancelUI maybeTarget env |> Undo.swap undoModel
    -- feature modules
    Tool toolMsg -> Tool.update toolMsg env
    Text textMsg -> Text.update textMsg env
    Mouse mouseMsg -> Mouse.update mouseMsg env
    Search searchMsg -> Search.update searchMsg env
    Icon iconMenuMsg -> Icon.update iconMenuMsg undoModel
    Nav navMsg -> Nav.update navMsg env
    --
    Scrolled pos -> updateScrollPos pos present |> S.store |> Undo.swap undoModel
    UrlResolved (imageId, url) -> cacheImageUrl imageId url present |> Undo.swap undoModel
    NoOp -> (undoModel, Cmd.none)


-- Presumption: both players exist in same box
createAssoc : Id -> Id -> BoxId -> Model -> Model
createAssoc player1 player2 boxId model =
  createAssocAndAddToBox Crosslink player1 player2 boxId model


-- Presumption: both players exist in same box
createAssocAndAddToBox : AssocType -> Id -> Id -> BoxId -> Model -> Model
createAssocAndAddToBox assocType player1 player2 boxId model =
  let
    (newModel, assocId) = Item.createAssoc assocType player1 player2 model
    expansion = Collapsed
    props = AssocP AssocProps
  in
  newModel
    |> Box.addItem (BoxItem assocId expansion) boxId
    |> TM.addItem assocId props boxId -- TODO: don't operate on "topicMap" directly


moveTopicToBox : Id -> BoxId -> Point -> BoxId -> BoxPath -> Point -> Env -> Model
moveTopicToBox topicId boxId origPos targetBoxId targetPath pos ({model} as env) =
  case (TM.topicProps topicId boxId model, Box.expansionOf topicId boxId model) of
    (Just topicProps, Just expansion) ->
      model
        |> Box.addItem (BoxItem topicId expansion) targetBoxId
        |> TM.removeItem topicId boxId
        |> TM.setTopicPos topicId boxId origPos
        |> TM.addItem topicId (TopicP { topicProps | pos = pos }) targetBoxId
        |> Sel.select targetBoxId targetPath
        |> Env.autoSize env
    _ -> model


select : Id -> BoxPath -> Model -> (Model, Cmd Msg)
select itemId boxPath model =
  ( model |> Sel.select itemId boxPath
  , Cmd.none
  )


cancelUI : Maybe Target -> Env -> (Model, Cmd Msg)
cancelUI maybeTarget ({model} as env) =
  model
    |> Icon.closePicker
    |> Search.closeMenu
    |> Tool.closeMenu
    |> Env.withModel env
    |> cancelUIWith maybeTarget


cancelUIWith : Maybe Target -> Env -> (Model, Cmd Msg)
cancelUIWith maybeTarget ({model} as env) =
  let
    isTargeted =
      case maybeTarget of
        Just (itemId, boxId :: _) ->
          Sel.isSelected itemId boxId model ||
          Mouse.isHovered itemId boxId model
        _ -> False
  in
  if isTargeted then
    ( model, Cmd.none ) -- keep selection, hover state, and edit mode
  else
    model
      |> Sel.clear
      |> Mouse.clearHover
      |> Env.withModel env
      |> Text.leaveEdit


updateScrollPos : Point -> Model -> Model
updateScrollPos pos model =
  TM.updateScrollPos model.boxId (\_ -> pos) model


cacheImageUrl : ImageId -> String -> Model -> (Model, Cmd Msg)
cacheImageUrl imageId url model =
  ( { model | imageCache = model.imageCache |> Dict.insert imageId url }
  , Cmd.none
  )
