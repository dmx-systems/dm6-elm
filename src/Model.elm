module Model exposing (Model, Msg(..), init, resetTransient, encode, decoder, map)

import Config as C
import ModelBase exposing (..)
import Renderer
-- box renderers
import TopicList.TopicListDef as TopicListDef
import TopicMap.TopicMapDef as TopicMapDef
-- feature modules
import Feature.IconDef as IconDef
import Feature.IdDef as IdDef
import Feature.MouseDef as MouseDef
import Feature.NavDef as NavDef
import Feature.SearchDef as SearchDef
import Feature.SelDef as SelDef
import Feature.SyncDef as SyncDef
import Feature.TextDef as TextDef
import Feature.ToolDef as ToolDef

import Dict exposing (Dict)
import Json.Decode as D
import Json.Decode.Pipeline exposing (required, hardcoded)
import Json.Encode as E



type alias Model =
  { assocs : Dict Id Assoc
  , itemSets: Dict Id ItemSet
  , boxes : Dict Id Box
  , boxId : BoxId -- the box rendered fullscreen
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
  , sync : SyncDef.Model
  , id : IdDef.Model
  }


init : Model
init =
  { assocs = Dict.empty
  , itemSets = Dict.singleton "TODO-1" <| ItemSet "TODO-1" []
  , boxes = Dict.singleton
      (toBoxId rootBoxId)
      (Box rootBoxId "TODO-1" Dict.empty Renderer.default)
  , boxId = rootBoxId
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
  , sync = SyncDef.init
  , id = IdDef.init
  }


resetTransient : Model -> Model
resetTransient model =
  { model
    | mouse = model.mouse |> MouseDef.resetTransient
    , topicList = model.topicList |> TopicListDef.resetTransient
    -- TODO: add other modules as well
  }


type Msg
  -- box renderers
  = TopicMap TopicMapDef.Msg
  -- feature modules
  | Tool ToolDef.Msg
  | Text TextDef.Msg
  | Mouse MouseDef.Msg
  | Search SearchDef.Msg
  | Icon IconDef.Msg
  | Nav NavDef.Msg
  | Id IdDef.Msg
  --
  | Scrolled Point
  | Cancel (Maybe Target)
  | NoOp



-- JSON


encode : Model -> E.Value
encode model =
  E.object
    [ ("assocs", model.assocs |> Dict.values |> E.list encodeAssoc)
    , ("itemSets", model.itemSets |> Dict.values |> E.list encodeItemSet)
    , ("boxes", model.boxes |> Dict.values |> E.list encodeBox)
    , ("boxId", encodeBoxId model.boxId)
    -- box renderers
    , ("topicMap", TopicMapDef.encode model.topicMap)
    , ("topicList", TopicListDef.encode model.topicList)
    -- feature modules
    , ("tool", ToolDef.encode model.tool)
    ]


decoder : D.Decoder Model
decoder =
  D.succeed Model
    |> required "assocs" (toDictDecoderWith toAssocId assocDecoder)
    |> required "itemSets" (toDictDecoder itemSetDecoder)
    |> required "boxes" (toDictDecoderWith toBoxId boxDecoder)
    |> required "boxId" boxIdDecoder
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
    |> hardcoded SyncDef.init
    |> hardcoded IdDef.init



-- API


map : (Model -> Model) -> (Model, Cmd Msg) -> (Model, Cmd Msg)
map transform (model, cmd) =
  (transform model, cmd)
