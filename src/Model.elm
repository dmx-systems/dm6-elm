module Model exposing (..)

import Config as C
import Feature.Icon as Icon
import Feature.Mouse as Mouse
import Feature.Nav as Nav
import Feature.Search as Search
import Feature.Selection as Selection
import Feature.TextEdit as TextEdit
import Feature.Tool as Tool
import ModelParts exposing (..)

import Browser.Navigation exposing (Key)
import Dict
import Json.Decode as D
import Json.Decode.Pipeline exposing (required, hardcoded)
import Json.Encode as E
import Set



type alias Model =
  { items : Items
  , boxes : Boxes
  , boxId : BoxId
  , nextId : Id
  ----- transient -----
  -- feature modules
  , edit : TextEdit.Model
  , mouse : Mouse.Model
  , search : Search.Model
  , icon : Icon.Model
  , selection : Selection.Model
  , nav : Nav.Model
  }


init : Key -> Model
init key =
  { items = Dict.singleton 0 <| Item 0 (Topic (TopicInfo 0 C.rootBoxName Nothing)) Set.empty
  , boxes = Dict.singleton rootBoxId
    <| Box rootBoxId (Rectangle 0 0 0 0) Dict.empty
  , boxId = rootBoxId
  , nextId = 1
  ----- transient -----
  -- feature modules
  , edit = TextEdit.init
  , mouse = Mouse.init
  , search = Search.init
  , icon = Icon.init
  , selection = Selection.init
  , nav = Nav.init key
  }


initTransient : Model -> Model
initTransient model =
  { model
  ----- transient -----
  -- feature modules
  | edit = TextEdit.init
  , mouse = Mouse.init
  , search = Search.init
  , icon = Icon.init
  , selection = Selection.init
  }


type Msg
  = AddAssoc Id Id BoxId
  | MoveTopicToBox Id BoxId Point Id BoxPath Point -- start point, random point (for target)
  | DraggedTopic
  | ClickedItem Id BoxPath
  | ClickedBackground
  | NoOp
  -- feature modules
  | Tool Tool.Msg
  | Edit TextEdit.Msg
  | Mouse Mouse.Msg
  | Search Search.Msg
  | Icon Icon.Msg
  | Nav Nav.Msg



-- JSON


encode : Model -> E.Value
encode model =
  E.object
    [ ("items", model.items |> Dict.values |> E.list encodeItem)
    , ("boxes", model.boxes |> Dict.values |> E.list encodeBox)
    , ("boxId", E.int model.boxId)
    , ("nextId", E.int model.nextId)
    ]


decoder : Key -> D.Decoder Model
decoder key =
  D.succeed Model
  |> required "items" (D.list itemDecoder |> D.andThen toDictDecoder)
  |> required "boxes" (D.list boxDecoder |> D.andThen toDictDecoder)
  |> required "boxId" D.int
  |> required "nextId" D.int
  ----- transient -----
  -- feature modules
  |> hardcoded TextEdit.init
  |> hardcoded Mouse.init
  |> hardcoded Search.init
  |> hardcoded Icon.init
  |> hardcoded Selection.init
  |> hardcoded (Nav.init key)
