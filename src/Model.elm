module Model exposing (Model, Msg(..), TopicHandler, AssocHandler, init, resetTransient, encode,
  decoder, map, nextId, generateId)

import Config as C
import ModelBase exposing (..)
import Renderer
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
import Random



type alias Model =
  { topics : Dict Id Topic
  , assocs : Dict Id Assoc
  , itemSets: Dict Id ItemSet
  , boxes : Dict Id Box
  , boxId : BoxId -- the box rendered fullscreen
  --, nextId : Id -- ### TODO
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
    rootTopic = Topic (TopicId "TODO-0") Nothing C.rootBoxName (TextSize (Size 0 0) (Size 0 0)) []
  in
  { topics = Dict.singleton "TODO-0" rootTopic
  , assocs = Dict.empty
  , itemSets = Dict.singleton "TODO-1" <| ItemSet "TODO-1" []
  , boxes = Dict.singleton
      (toBoxId rootBoxId)
      (Box rootBoxId "TODO-1" Dict.empty Renderer.default)
  , boxId = rootBoxId
  --, nextId = "### TODO-2"
  -- box renderers
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


resetTransient : Model -> Model
resetTransient model =
  { model
    | mouse = model.mouse |> MouseDef.resetTransient
    , topicList = model.topicList |> TopicListDef.resetTransient
    -- TODO: add other modules as well
  }


type Msg
  = CreateTopic Id String (Maybe Icon) TopicHandler
  | CreateAssoc Id AssocType TopicId TopicId AssocHandler
  -- box renderers
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
  | Cancel (Maybe Target)
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
    --, ("nextId", E.int model.nextId) -- ### TODO
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
    -- |> required "nextId" D.int -- ### TODO
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


map : (Model -> Model) -> (Model, Cmd Msg) -> (Model, Cmd Msg)
map transform (model, cmd) =
  (transform model, cmd)


-- TODO: drop
nextId : Model -> Model
nextId model =
  model
  -- { model | nextId = model.nextId + 1 } -- ### TODO


-- TODO: use UUID generator
idGenerator : Random.Generator Id
idGenerator =
  Random.int Random.minInt Random.maxInt
    |> Random.map String.fromInt


generateId : (Id -> msg) -> Cmd msg
generateId toMsg =
  Random.generate toMsg idGenerator


-- ID Handler

type alias TopicHandler =
  Topic -> Model -> (Model, Cmd Msg)


type alias AssocHandler =
  Assoc -> Model -> Model
