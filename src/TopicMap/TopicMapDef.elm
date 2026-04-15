module TopicMap.TopicMapDef exposing (Model, TopicMap, MapItems, MapItem, Visibility(..),
  ItemProps(..), TopicProps, AssocProps, init, encode, decoder)

import ModelBase exposing (..)
import Dict exposing (Dict)
import Json.Decode as D
import Json.Encode as E



type alias Model = Dict BoxId TopicMap


init : Model
init =
  Dict.singleton rootBoxId <| TopicMap rootBoxId (Rectangle 0 0 0 0) (Point 0 0) Dict.empty


type alias TopicMap =
  { id : BoxId
  , rect : Rectangle
  , scroll : Point
  , items : MapItems
  }


type alias MapItems = Dict Id MapItem


type alias MapItem =
  { id : Id
  , visibility : Visibility
  , props : ItemProps
  }


type Visibility
  = Visible
  | Removed


type ItemProps
  = TopicP TopicProps
  | AssocP AssocProps


type alias TopicProps =
  { pos : Point
  , expansion : Expansion -- transient render state? ### TODO: drop?
  }


type alias AssocProps =
  {}



-- JSON


-- Encode

encode : Model -> E.Value
encode model =
  model
    |> Dict.values
    |> E.list encodeTopicMap


encodeTopicMap : TopicMap -> E.Value
encodeTopicMap map =
  E.object
    [ ("id", E.int map.id)
    , ("rect", E.object
        [ ("x1", E.int map.rect.x1)
        , ("y1", E.int map.rect.y1)
        , ("x2", E.int map.rect.x2)
        , ("y2", E.int map.rect.y2)
        ]
      )
    , ("scroll", E.object
        [ ("x", E.int map.scroll.x)
        , ("y", E.int map.scroll.y)
        ]
      )
    , ("items", map.items |> Dict.values |> E.list encodeMapItem)
    ]


encodeMapItem : MapItem -> E.Value
encodeMapItem item =
  E.object
    [ ("id", E.int item.id)
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
            , ("expansion", encodeExpansion topicProps.expansion)
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
      Visible -> "Visible"
      Removed -> "Removed"


-- Decode

decoder : D.Decoder Model
decoder =
  topicMapDecoder |> toDictDecoder


topicMapDecoder : D.Decoder TopicMap
topicMapDecoder =
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
    (D.field "items" (mapItemDecoder |> toDictDecoder))


mapItemDecoder : D.Decoder MapItem
mapItemDecoder =
  D.map3 MapItem
    (D.field "id" D.int)
    (D.field "visibility" D.string |> D.andThen visibilityDecoder)
    (D.oneOf
      [ D.field "topicProps" <| D.map TopicP <| D.map2 TopicProps
        (D.field "pos" <| D.map2 Point
          (D.field "x" D.int)
          (D.field "y" D.int)
        )
        (D.field "expansion" D.string |> D.andThen expansionDecoder)
      , D.field "assocProps" <| D.succeed (AssocP AssocProps)
      ]
    )


visibilityDecoder : String -> D.Decoder Visibility
visibilityDecoder str =
  case str of
    "Visible" -> D.succeed Visible
    "Removed" -> D.succeed Removed
    _ -> D.fail <| "\"" ++ str ++ "\" is an invalid Visibility"
