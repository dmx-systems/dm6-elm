module Model exposing (Model, Msg(..), init, encode, decoder, nextId)

import Config as C
import Extension
import ModelBase exposing (..)
-- box renderers
import TopicList.TopicListDef as TopicListDef
import TopicMap.TopicMapDef as TopicMapDef
-- feature modules
import Feature.IconDef as IconDef
import Feature.MouseDef as MouseDef
import Feature.NavDef as NavDef
import Feature.SearchDef as SearchDef
import Feature.SelDef as SelDef
import Feature.TextDef as TextDef
import Feature.ToolDef as ToolDef

import Dict exposing (Dict)
import Json.Decode as D
import Json.Decode.Pipeline exposing (required, hardcoded)
import Json.Encode as E



type alias Model =
  { topics : Dict Id Topic
  , assocs : Dict Id Assoc
  , itemSets: Dict Id ItemSet
  , boxes : Dict Id Box
  , boxId : BoxId -- the box rendered fullscreen
  , nextId : Id
  -- box renderers
  , topicMap : TopicMapDef.Model
  , topicList : TopicListDef.Model
  -- feature modules
  , tool : ToolDef.Model
  , text : TextDef.Model
  , mouse : MouseDef.Model
  , search : SearchDef.Model
  , icon : IconDef.Model
  , selection : SelDef.Model
  }


init : Model
init =
  let
    rootTopic = Topic (TopicId 0) Nothing C.rootBoxName (TextSize (Size 0 0) (Size 0 0)) []
  in
  { topics = Dict.singleton 0 rootTopic
  , assocs = Dict.empty
  , itemSets = Dict.singleton 1 <| ItemSet 1 []
  , boxes = Dict.singleton
      (toBoxId rootBoxId)
      (Box rootBoxId 1 Dict.empty Extension.defaultRenderer)
  , boxId = rootBoxId
  , nextId = 2
  -- renderer modules
  , topicMap = TopicMapDef.init
  , topicList = TopicListDef.init
  -- feature modules
  , tool = ToolDef.init
  , text = TextDef.init
  , mouse = MouseDef.init
  , search = SearchDef.init
  , icon = IconDef.init
  , selection = SelDef.init
  }


type Msg
  -- gestures detected by Mouse module
  = CreateAssoc TopicId TopicId BoxId
  | TopicDropped TopicId BoxId Point TopicId BoxPath
  | TopicDragged
  | ItemClicked ItemId BoxPath
  | Cancel (Maybe Target)
  -- renderer modules
  | TopicMap TopicMapDef.Msg
  -- feature modules
  | Tool ToolDef.Msg
  | Text TextDef.Msg
  | Mouse MouseDef.Msg
  | Search SearchDef.Msg
  | Icon IconDef.Msg
  | Nav NavDef.Msg
  --
  | Scrolled Point
  | NoOp



-- JSON


encode : Model -> E.Value
encode model =
  E.object
    [ ("topics", model.topics |> Dict.values |> E.list encodeTopic)
    , ("assocs", model.assocs |> Dict.values |> E.list encodeAssoc)
    , ("itemSets", model.itemSets |> Dict.values |> E.list encodeItemSet)
    , ("boxes", model.boxes |> Dict.values |> E.list encodeBox)
    , ("boxId", encodeBoxId model.boxId)
    , ("nextId", E.int model.nextId)
    -- box renderers
    , ("topicMap", TopicMapDef.encode model.topicMap)
    , ("topicList", TopicListDef.encode model.topicList)
    -- feature modules
    , ("tool", ToolDef.encode model.tool)
    ]


decoder : D.Decoder Model
decoder =
  D.succeed Model
    |> required "topics" (toDictDecoderWith toTopicId topicDecoder)
    |> required "assocs" (toDictDecoderWith toAssocId assocDecoder)
    |> required "itemSets" (toDictDecoder itemSetDecoder)
    |> required "boxes" (toDictDecoderWith toBoxId boxDecoder)
    |> required "boxId" boxIdDecoder
    |> required "nextId" D.int
    -- box renderers
    |> required "topicMap" TopicMapDef.decoder
    |> required "topicList" TopicListDef.decoder
    -- feature modules
    |> required "tool" ToolDef.decoder
    |> hardcoded TextDef.init
    |> hardcoded MouseDef.init
    |> hardcoded SearchDef.init
    |> hardcoded IconDef.init
    |> hardcoded SelDef.init



-- API


nextId : Model -> Model
nextId model =
  { model | nextId = model.nextId + 1 }
