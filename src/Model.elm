module Model exposing (Model, Msg(..), BootMode(..), bootstrap, encode, decode, init, map, cmd)

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

import Crdt.Doc exposing (Doc)

import Json.Decode as D
import Json.Encode as E



{- Local state, not synced. ### FIXDOC
A certain portion is persistent (see `encode` below), the remainder is transient.
Additionally holds a materialized form (`cache`) of the synced state.
-}
type alias Model =
  { cache : SyncModel -- the materialized CRDT document, for reading
  , docId : String -- ID of the CRDT document being edited
  , boxId : BoxId -- the box rendered fullscreen
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


{- Creates the initial empty Model when no DB exists.
-}
bootstrap : String -> String -> TopicId -> Model
bootstrap replicaId docId rootTopicId =
  { cache = SyncModel.bootstrap
  , docId = docId
  , boxId = BoxId rootTopicId
  -- box renderers
  , topicMap = TopicMapDef.init
  , topicList = TopicListDef.init
  -- feature modules
  , crdt = CrdtDef.bootstrap replicaId
  , tool = ToolDef.init
  , text = TextDef.init
  , mouse = MouseDef.init
  , search = SearchDef.init
  , icon = IconDef.init
  , selection = SelDef.init
  , id = IdDef.init
  }


{- Creates the initial Running model -}
init : String -> LocalDocState -> Doc SyncModel -> SyncModel -> Model
init docId docState doc docModel =
  Model
    docModel
    docId
    docState.boxId
    -- box renderers
    TopicMapDef.init
    TopicListDef.init
    -- feature modules
    (CrdtDef.init doc)
    docState.tool
    TextDef.init
    MouseDef.init
    SearchDef.init
    IconDef.init
    SelDef.init
    IdDef.init


type Msg
  -- box renderers
  = TopicMap TopicMapDef.Msg
  -- feature modules
  | Crdt CrdtDef.Msg
  | Tool ToolDef.Msg
  | Text TextDef.Msg
  | Mouse MouseDef.Msg
  | Search SearchDef.Msg
  | Icon IconDef.Msg
  | Nav NavDef.Msg
  | Id IdDef.Msg
  --
  | Bootstrap BootMode String (List Id) -- initial URL #hash, initial ID pool
  | Scrolled Point
  | Cancel (Maybe Target)
  | NoOp


type BootMode
  = Local
  | Server


{- State compiled from environment (local DB, URL), for handing over to Elm initialization.
Not created for `Bootstrap` (no local DB, no URL parameters).
-}
type alias LaunchState =
  { replicaId : Maybe String        -- Not available for `ServerBootstrap`
  , docId : String
  , docState : Maybe LocalDocState  -- Not available for `Server` and `ServerBootstrap`
  , doc : Maybe E.Value             -- Not available for `Server` and `ServerBootstrap`
  }


type alias LocalDocState =
  { boxId : BoxId
  , tool : ToolDef.Model
  }



-- JSON


{- Encodes the Model portion to be stored in the "local" object store.
-}
encode : Model -> E.Value
encode model =
  E.object
    [ ("docId", E.string model.docId)
    , ("boxId", encodeBoxId model.boxId)
    -- feature modules
    , ("tool", ToolDef.encode model.tool)
    ]


decode : E.Value -> Result D.Error LaunchState
decode launchState =
  let
    launchStateDecoder =
      D.map4 LaunchState
        (D.field "replicaId" (D.nullable D.string))
        (D.field "docId" D.string)
        (D.field "docState" (D.nullable docStateDecoder))
        (D.field "doc" (D.nullable D.value))


    docStateDecoder =
      D.map2 LocalDocState
        (D.field "boxId" boxIdDecoder)
        (D.field "tool" ToolDef.decoder)
  in
  launchState
    |> D.decodeValue launchStateDecoder



-- Model-Cmd tuple


map : (Model -> Model) -> (Model, Cmd Msg) -> (Model, Cmd Msg)
map transform (model, cmd_) =
  (transform model, cmd_)


cmd : Cmd Msg -> Model -> (Model, Cmd Msg)
cmd cmd_ model =
  (model, cmd_)
