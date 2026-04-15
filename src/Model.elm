module Model exposing (Model, Msg(..), init, encode, decoder)

import Config as C
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
import Set



type alias Model =
  { items : Dict Id Item
  , itemSets: Dict Id ItemSet
  , boxes : Dict BoxId Box
  , boxId : BoxId -- the box rendered fullscreen
  , nextId : Id
  , imageCache : Dict ImageId String -- Int -> blob: URL ### TODO: move to Text module
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
    rootTopic = TopicInfo 0 Nothing C.rootBoxName <| TextSize (Size 0 0) (Size 0 0)
  in
  { items = Dict.singleton 0 <| Item 0 (Topic rootTopic) Set.empty
  , itemSets = Dict.singleton 1 <| ItemSet 1 []
  , boxes = Dict.singleton rootBoxId <| Box rootBoxId 1 Dict.empty "TopicMap"
  , boxId = rootBoxId
  , nextId = 2
  , imageCache = Dict.empty -- TODO: move to Text module, but should survive a map switch
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
  = CreateAssoc Id Id BoxId
  | MoveTopicToBox Id BoxId Point Id BoxPath Point -- start point, random point (for target)
  | TopicDragged
  | ItemClicked Id BoxPath
  | Cancel (Maybe Target)
  -- feature modules
  | Tool ToolDef.Msg
  | Text TextDef.Msg
  | Mouse MouseDef.Msg
  | Search SearchDef.Msg
  | Icon IconDef.Msg
  | Nav NavDef.Msg
  --
  | Scrolled Point
  | UrlResolved (ImageId, String)
  | NoOp



-- JSON


encode : Model -> E.Value
encode model =
  E.object
    [ ("items", model.items |> Dict.values |> E.list encodeItem)
    , ("itemSets", model.itemSets |> Dict.values |> E.list encodeItemSet)
    , ("boxes", model.boxes |> Dict.values |> E.list encodeBox)
    , ("boxId", E.int model.boxId)
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
    |> required "items" (itemDecoder |> toDictDecoder)
    |> required "itemSets" (itemSetDecoder |> toDictDecoder)
    |> required "boxes" (boxDecoder |> toDictDecoder)
    |> required "boxId" D.int
    |> required "nextId" D.int
    |> hardcoded Dict.empty -- imageCache
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
