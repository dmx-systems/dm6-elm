port module Storage exposing (store, storeWith, modelDecoder, importJSON, exportJSON)

import AppModel exposing (..)
import Model exposing (..)

import Dict exposing (Dict)
import Json.Decode as D
import Json.Decode.Pipeline exposing (required, hardcoded)
import Json.Encode as E
import Set



-- PORTS


port storeModel : E.Value -> Cmd msg
port importJSON : () -> Cmd msg
port exportJSON : () -> Cmd msg


--

store : Model -> (Model, Cmd Msg)
store model =
  (model, encodeModel model |> storeModel)


storeWith : (Model, Cmd Msg) -> (Model, Cmd Msg)
storeWith (model, cmd) =
  ( model
  , Cmd.batch
    [ cmd
    , encodeModel model |> storeModel
    ]
  )



-- ENCODE/DECODE MODEL <-> JS VALUE


-- Encode

encodeModel : Model -> E.Value
encodeModel model =
  E.object
    [ ("items", model.items |> Dict.values |> E.list encodeItem)
    , ("boxes", model.boxes |> Dict.values |> E.list encodeBox)
    , ("boxPath", E.list E.int model.boxPath)
    , ("nextId", E.int model.nextId)
    ]


encodeItem : Item -> E.Value
encodeItem item =
  E.object
    [ case item.info of
      Topic topic ->
        ( "topic"
        , E.object
          [ ("id", E.int topic.id)
          , ("text", E.string topic.text)
          , ("icon", E.string <| Maybe.withDefault "" topic.iconName)
          , ("assocIds", E.set E.int item.assocIds)
          ]
        )
      Assoc assoc ->
        ( "assoc"
        , E.object
          [ ("id", E.int assoc.id)
          , ("type", E.string assoc.itemType)
          , ("role1", E.string assoc.role1)
          , ("player1", E.int assoc.player1)
          , ("role2", E.string assoc.role2)
          , ("player2", E.int assoc.player2)
          , ("assocIds", E.set E.int item.assocIds)
          ]
        )
    ]


encodeBox : Box -> E.Value
encodeBox box =
  E.object
    [ ("id", E.int box.id)
    , ("rect", E.object
        [ ("x1", E.float box.rect.x1)
        , ("y1", E.float box.rect.y1)
        , ("x2", E.float box.rect.x2)
        , ("y2", E.float box.rect.y2)
        ]
      )
    , ("items", box.items |> Dict.values |> E.list encodeBoxItem)
    ]


encodeBoxItem : BoxItem -> E.Value
encodeBoxItem item =
  E.object
    [ ("id", E.int item.id)
    , ("parentAssocId", E.int item.parentAssocId)
    , ("hidden", E.bool item.hidden)
    , ("pinned", E.bool item.pinned)
    , case item.props of
        TopicV topicProps ->
          ( "topicProps"
          , E.object
            [ ("pos", E.object
                [ ("x", E.float topicProps.pos.x)
                , ("y", E.float topicProps.pos.y)
                ]
              )
            , ("size", E.object
                [ ("w", E.float topicProps.size.w)
                , ("h", E.float topicProps.size.h)
                ]
              )
            , ("display", encodeDisplayName topicProps.displayMode)
            ]
          )
        AssocV assosProps ->
          ( "assocProps"
          , E.object []
          )
    ]


encodeDisplayName : DisplayMode -> E.Value
encodeDisplayName displayMode =
  E.string
    (case displayMode of
      TopicD LabelOnly -> "LabelOnly"
      TopicD Detail -> "Detail"
      BoxD BlackBox -> "BlackBox"
      BoxD WhiteBox -> "WhiteBox"
      BoxD Unboxed -> "Unboxed"
    )


-- Decode

modelDecoder : D.Decoder Model
modelDecoder =
  D.succeed Model
    |> required "items" (D.list itemDecoder |> D.andThen toDictDecoder)
    |> required "boxes" (D.list boxDecoder |> D.andThen toDictDecoder)
    |> required "boxPath" (D.list D.int)
    |> required "nextId" D.int
    ----- transient -----
    |> hardcoded default.selection
    |> hardcoded default.editState
    |> hardcoded default.measureText
    -- feature modules
    |> hardcoded default.mouse
    |> hardcoded default.search
    |> hardcoded default.iconMenu


itemDecoder : D.Decoder Item
itemDecoder =
  D.oneOf
    [ D.field "topic"
      (D.map3 Item
        (D.field "id" D.int)
        (D.map Topic <| D.map3 TopicInfo
          (D.field "id" D.int)
          (D.field "text" D.string)
          (D.field "icon" D.string
            |> D.andThen maybeString
          )
        )
        assocIdsDecoder
      )
    , D.field "assoc"
      (D.map3 Item
        (D.field "id" D.int)
        (D.map Assoc <| D.map6 AssocInfo
          (D.field "id" D.int)
          (D.field "type" D.string)
          (D.field "role1" D.string)
          (D.field "player1" D.int)
          (D.field "role2" D.string)
          (D.field "player2" D.int)
        )
        assocIdsDecoder
      )
    ]


assocIdsDecoder : D.Decoder AssocIds
assocIdsDecoder =
  D.field "assocIds" (D.list D.int)
  |> D.andThen (Set.fromList >> D.succeed)


boxDecoder : D.Decoder Box
boxDecoder =
  D.map3 Box
    (D.field "id" D.int)
    (D.field "rect" <| D.map4 Rectangle
      (D.field "x1" D.float)
      (D.field "y1" D.float)
      (D.field "x2" D.float)
      (D.field "y2" D.float)
    )
    (D.field "items" (D.list boxItemDecoder |> D.andThen toDictDecoder))


boxItemDecoder : D.Decoder BoxItem
boxItemDecoder =
  D.map5 BoxItem
    (D.field "id" D.int)
    (D.field "parentAssocId" D.int)
    (D.field "hidden" D.bool)
    (D.field "pinned" D.bool)
    (D.oneOf
      [ D.field "topicProps" <| D.map TopicV <| D.map3 TopicProps
        (D.field "pos" <| D.map2 Point
          (D.field "x" D.float)
          (D.field "y" D.float)
        )
        (D.field "size" <| D.map2 Size
          (D.field "w" D.float)
          (D.field "h" D.float)
        )
        (D.field "display" D.string |> D.andThen displayModeDecoder)
      , D.field "assocProps" <| D.succeed (AssocV AssocProps)
      ]
    )


toDictDecoder : List { item | id : Id } -> D.Decoder (Dict Int { item | id : Id })
toDictDecoder items =
  items
  |> List.map (\item -> (item.id, item))
  |> Dict.fromList
  |> D.succeed


displayModeDecoder : String -> D.Decoder DisplayMode
displayModeDecoder str =
  case str of
    "LabelOnly" -> D.succeed (TopicD LabelOnly)
    "Detail" -> D.succeed (TopicD Detail)
    "BlackBox" -> D.succeed (BoxD BlackBox)
    "WhiteBox" -> D.succeed (BoxD WhiteBox)
    "Unboxed" -> D.succeed (BoxD Unboxed)
    _ -> D.fail <| "\"" ++ str ++ "\" is an invalid display mode"


maybeString : String -> D.Decoder (Maybe String)
maybeString str =
  D.succeed
    ( case str of
        "" -> Nothing
        _ -> Just str
    )
