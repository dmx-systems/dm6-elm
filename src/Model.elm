module Model exposing (..)

import Config as C
import Feature.Icon as Icon
import Feature.Mouse as Mouse
import Feature.Nav as Nav
import Feature.Search as Search
import Feature.Sel as Sel
import Feature.Text as Text
import Feature.Tool as Tool
import ModelParts exposing (..)
import TopicMap.TopicMapDef as TopicMapDef

import Dict exposing (Dict)
import Json.Decode as D
import Json.Decode.Pipeline exposing (required, hardcoded)
import Json.Encode as E
import Set



type alias Model =
  { items : Items
  , itemSets: ItemSets
  , boxes : Boxes
  , boxId : BoxId -- the box rendered fullscreen
  , nextId : Id
  , imageCache : Dict ImageId String -- Int -> blob: URL
  -- renderer modules
  , topicMap : TopicMapDef.Model
  -- feature modules
  , tool : Tool.Model
  , text : Text.Model
  , mouse : Mouse.Model
  , search : Search.Model
  , icon : Icon.Model
  , selection : Sel.Model
  }


init : Model
init =
  let
    homeTopic = TopicInfo 0 Nothing C.homeBoxName <| TextSize (Size 0 0) (Size 0 0)
  in
  { items = Dict.singleton 0 <| Item 0 (Topic homeTopic) Set.empty
  , itemSets = Dict.singleton 1 <| ItemSet 1 []
  , boxes = Dict.singleton homeBoxId <| Box homeBoxId 1 Dict.empty
  , boxId = homeBoxId
  , nextId = 2
  , imageCache = Dict.empty -- TODO: move to Text module, but should survive a map switch
  -- renderer modules
  , topicMap = TopicMapDef.init
  -- feature modules
  , tool = Tool.init
  , text = Text.init
  , mouse = Mouse.init
  , search = Search.init
  , icon = Icon.init
  , selection = Sel.init
  }


type Msg
  -- gestures detected by Mouse module
  = CreateAssoc Id Id BoxId
  | MoveTopicToBox Id BoxId Point Id BoxPath Point -- start point, random point (for target)
  | TopicDragged
  | ItemClicked Id BoxPath
  | Cancel (Maybe (Id, BoxPath)) -- target
  -- feature modules
  | Tool Tool.Msg
  | Text Text.Msg
  | Mouse Mouse.Msg
  | Search Search.Msg
  | Icon Icon.Msg
  | Nav Nav.Msg
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
    -- renderer modules
    , ("topicMap", TopicMapDef.encode model.topicMap)
    -- feature modules
    , ("tool", Tool.encode model.tool)
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
    -- renderer modules
    |> required "topicMap" TopicMapDef.decoder
    -- feature modules
    |> required "tool" Tool.decoder
    |> hardcoded Text.init
    |> hardcoded Mouse.init
    |> hardcoded Search.init
    |> hardcoded Icon.init
    |> hardcoded Sel.init
