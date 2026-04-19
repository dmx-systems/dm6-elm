module TopicMap.TopicMapDef exposing (Model, TopicMap, MapItem, ItemProps(..),
  TopicProps, AssocProps, Msg(..), init, encode, decoder)

import ModelBase exposing (..)
import Dict exposing (Dict)
import Json.Decode as D
import Json.Encode as E



type alias Model =
  Dict BoxId TopicMap


init : Model
init =
  Dict.singleton rootBoxId <| TopicMap rootBoxId (Rectangle 0 0 0 0) (Point 0 0) Dict.empty


type alias TopicMap =
  { id : BoxId
  , rect : Rectangle
  , scroll : Point
  , items : Dict Id MapItem
  }


type alias MapItem =
  { id : Id
  , props : ItemProps
  }


type ItemProps
  = TopicP TopicProps
  | AssocP AssocProps


type alias TopicProps =
  { pos : Point
  , expansion : Expansion -- transient, for "limbo" rendering
  }


type alias AssocProps =
  {}


type Msg
  = AddTopic Id BoxId Point -- random pos



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
    , case item.props of
        TopicP topicProps ->
          ( "topicProps"
          , E.object
            [ ("pos", E.object
                [ ("x", E.int topicProps.pos.x)
                , ("y", E.int topicProps.pos.y)
                ]
              )
            , ("expansion", encodeExpansion topicProps.expansion) -- TODO: drop, transient?
            ]
          )
        AssocP assosProps ->
          ( "assocProps"
          , E.object []
          )
    ]


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
  D.map2 MapItem
    (D.field "id" D.int)
    (D.oneOf
      [ D.field "topicProps" <| D.map TopicP <| D.map2 TopicProps
        (D.field "pos" <| D.map2 Point
          (D.field "x" D.int)
          (D.field "y" D.int)
        )
        (D.field "expansion" D.string |> D.andThen expansionDecoder) -- TODO: hardcode value
      , D.field "assocProps" <| D.succeed (AssocP AssocProps)
      ]
    )
