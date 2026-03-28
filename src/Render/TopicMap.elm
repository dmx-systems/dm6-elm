module Render.TopicMap exposing (Model, TopicMap, MapItems, MapItem, Visibility(..),
  Pinned(..), ItemProps(..), TopicProps, AssocProps, DisplayMode(..), TopicDisplay(..),
  BoxDisplay(..), init, encodeBox, boxDecoder)

import ModelParts exposing (Id, BoxId, Point, Rectangle, homeBoxId, toDictDecoder)
import Dict exposing (Dict)
import Json.Decode as D
import Json.Encode as E



type alias Model =
  { topicMaps : TopicMaps }


init : Model
init =
  { topicMaps = Dict.singleton homeBoxId
    <| TopicMap homeBoxId (Rectangle 0 0 0 0) (Point 0 0) Dict.empty
  }


type alias TopicMaps = Dict BoxId TopicMap


type alias TopicMap =
  { id : BoxId
  , rect : Rectangle
  , scroll : Point
  , items : MapItems
  }


type alias MapItems = Dict Id MapItem


type alias MapItem =
  { id : Id
  , boxAssocId : Id
  , visibility : Visibility
  , props : ItemProps
  }


type Visibility
  = Visible Pinned
  | Removed


type Pinned
  = Pinned
  | Unpinned


type ItemProps
  = TopicP TopicProps
  | AssocP AssocProps


type alias TopicProps =
  { pos : Point
  , displayMode : DisplayMode -- serialized as "display", TODO: rename to "display"?
  }


type alias AssocProps =
  {}


type DisplayMode
  = TopicD TopicDisplay
  | BoxD BoxDisplay


type TopicDisplay
  = LabelOnly
  | Detail


type BoxDisplay
  = BlackBox
  | WhiteBox
  | Unboxed



-- JSON


-- Encode

encodeBox : TopicMap -> E.Value
encodeBox box =
  E.object
    [ ("id", E.int box.id)
    , ("rect", E.object
        [ ("x1", E.int box.rect.x1)
        , ("y1", E.int box.rect.y1)
        , ("x2", E.int box.rect.x2)
        , ("y2", E.int box.rect.y2)
        ]
      )
    , ("scroll", E.object
        [ ("x", E.int box.scroll.x)
        , ("y", E.int box.scroll.y)
        ]
      )
    , ("items", box.items |> Dict.values |> E.list encodeBoxItem)
    ]


encodeBoxItem : MapItem -> E.Value
encodeBoxItem item =
  E.object
    [ ("id", E.int item.id)
    , ("boxAssocId", E.int item.boxAssocId)
    , ("visibility", encodeVisibility item.visibility)
    , case item.props of
        TopicP topicProps ->
          ( "topicProps"
          , E.object
            [ ("pos", E.object
                [ ("x", E.int topicProps.pos.x)
                , ("y", E.int topicProps.pos.y)
                ]
              )
            , ("display", encodeDisplayName topicProps.displayMode)
            ]
          )
        AssocP assosProps ->
          ( "assocProps"
          , E.object []
          )
    ]


encodeVisibility : Visibility -> E.Value
encodeVisibility visibility =
  E.string <|
    case visibility of
      Visible Pinned -> "Pinned"
      Visible Unpinned -> "Visible"
      Removed -> "Removed"


encodeDisplayName : DisplayMode -> E.Value
encodeDisplayName displayMode =
  E.string <|
    case displayMode of
      TopicD LabelOnly -> "LabelOnly"
      TopicD Detail -> "Detail"
      BoxD BlackBox -> "BlackBox"
      BoxD WhiteBox -> "WhiteBox"
      BoxD Unboxed -> "Unboxed"


-- Decode

boxDecoder : D.Decoder TopicMap
boxDecoder =
  D.map4 TopicMap
    (D.field "id" D.int)
    (D.field "rect" <| D.map4 Rectangle
      (D.field "x1" D.int)
      (D.field "y1" D.int)
      (D.field "x2" D.int)
      (D.field "y2" D.int)
    )
    (D.field "scroll" <| D.map2 Point
      (D.field "x" D.int)
      (D.field "y" D.int)
    )
    (D.field "items" (D.list boxItemDecoder |> D.andThen toDictDecoder))


boxItemDecoder : D.Decoder MapItem
boxItemDecoder =
  D.map4 MapItem
    (D.field "id" D.int)
    (D.field "boxAssocId" D.int)
    (D.field "visibility" D.string |> D.andThen visibilityDecoder)
    (D.oneOf
      [ D.field "topicProps" <| D.map TopicP <| D.map2 TopicProps
        (D.field "pos" <| D.map2 Point
          (D.field "x" D.int)
          (D.field "y" D.int)
        )
        (D.field "display" D.string |> D.andThen displayModeDecoder)
      , D.field "assocProps" <| D.succeed (AssocP AssocProps)
      ]
    )


visibilityDecoder : String -> D.Decoder Visibility
visibilityDecoder str =
  case str of
    "Pinned" -> D.succeed (Visible Pinned)
    "Visible" -> D.succeed (Visible Unpinned)
    "Removed" -> D.succeed (Removed)
    _ -> D.fail <| "\"" ++ str ++ "\" is an invalid Visibility"


displayModeDecoder : String -> D.Decoder DisplayMode
displayModeDecoder str =
  case str of
    "LabelOnly" -> D.succeed (TopicD LabelOnly)
    "Detail" -> D.succeed (TopicD Detail)
    "BlackBox" -> D.succeed (BoxD BlackBox)
    "WhiteBox" -> D.succeed (BoxD WhiteBox)
    "Unboxed" -> D.succeed (BoxD Unboxed)
    _ -> D.fail <| "\"" ++ str ++ "\" is an invalid DisplayMode"
