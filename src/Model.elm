module Model exposing (Model, Msg(..), bootstrap, encode, decode, map, mergeCmd)

import ModelBase exposing (..)
import SyncModel exposing (SyncModel)
-- box renderers
import TopicList.TopicListDef as TopicListDef
import TopicMap.TopicMapDef as TopicMapDef
-- feature modules
import Feature.CrdtDef as CrdtDef
import Feature.IconDef as IconDef
import Feature.IdDef as IdDef
import Feature.MouseDef as MouseDef
import Feature.NavDef as NavDef
import Feature.SearchDef as SearchDef
import Feature.SelDef as SelDef
import Feature.TextDef as TextDef
import Feature.ToolDef as ToolDef

import Crdt.Doc exposing (Doc) -- TODO: drop

import Json.Decode as D
import Json.Decode.Pipeline exposing (required, hardcoded)
import Json.Encode as E



type alias Model =
  { cache : SyncModel -- the materialzed CRDT document, for reading
  , boxId : BoxId -- the box rendered fullscreen
  , rootBoxId : BoxId
  -- box renderers
  , topicMap : TopicMapDef.Model
  , topicList : TopicListDef.Model
  -- feature modules
  , crdt : CrdtDef.Model
  , tool : ToolDef.Model
  , text : TextDef.Model
  , mouse : MouseDef.Model
  , search : SearchDef.Model
  , icon : IconDef.Model
  , selection : SelDef.Model
  , id : IdDef.Model
  }


{- Creates the initial Model when no DB exists.
-}
bootstrap : TopicId -> Model
bootstrap rootTopicId =
  { cache = SyncModel.init
  , boxId = BoxId rootTopicId
  , rootBoxId = BoxId rootTopicId
  -- box renderers
  , topicMap = TopicMapDef.init
  , topicList = TopicListDef.init
  -- feature modules
  , crdt = CrdtDef.bootstrap
  , tool = ToolDef.init
  , text = TextDef.init
  , mouse = MouseDef.init
  , search = SearchDef.init
  , icon = IconDef.init
  , selection = SelDef.init
  , id = IdDef.init
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
  | Bootstrap String (List Id) -- initial URL #hash, initial ID pool
  | Scrolled Point
  | Cancel (Maybe Target)
  | NoOp



-- JSON


encode : Model -> E.Value
encode model =
  E.object
    [ ("boxId", encodeBoxId model.boxId)
    , ("rootBoxId", encodeBoxId model.rootBoxId)
    -- feature modules
    , ("tool", ToolDef.encode model.tool)
    ]


{- Creates the initial Model from DB content. -}
decode : E.Value -> (Doc SyncModel, SyncModel) -> Result D.Error Model
decode flags (doc, docModel) =
  flags |> D.decodeValue
    (D.succeed Model
      |> hardcoded docModel
      |> required "boxId" boxIdDecoder
      |> required "rootBoxId" boxIdDecoder
      -- box renderers
      |> hardcoded TopicMapDef.init
      |> hardcoded TopicListDef.init
      -- feature modules
      |> hardcoded (CrdtDef.init doc)
      |> required "tool" ToolDef.decoder
      |> hardcoded TextDef.init
      |> hardcoded MouseDef.init
      |> hardcoded SearchDef.init
      |> hardcoded IconDef.init
      |> hardcoded SelDef.init
      |> hardcoded IdDef.init
    )



-- API


map : (Model -> Model) -> (Model, Cmd Msg) -> (Model, Cmd Msg)
map transform (model, cmd) =
  (transform model, cmd)


mergeCmd : Cmd Msg -> (Model, Cmd Msg) -> (Model, Cmd Msg)
mergeCmd cmd_ (model, cmd) =
  (model, Cmd.batch [ cmd, cmd_ ])
