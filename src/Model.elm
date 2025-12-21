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

import Dict exposing (Dict)
import Json.Decode as D
import Json.Decode.Pipeline exposing (required, hardcoded)
import Json.Encode as E
import Set



type alias Model =
  { items : Items
  , boxes : Boxes
  , boxId : BoxId -- the box rendered fullscreen
  , nextId : Id
  ----- transient -----
  , imageCache : Dict ImageId String -- Int -> blob: URL
  -- feature modules
  , text : Text.Model
  , mouse : Mouse.Model
  , search : Search.Model
  , icon : Icon.Model
  , selection : Sel.Model
  }


init : Model
init =
  let
    rootTopic = TopicInfo 0 Nothing C.rootBoxName <| TextSize (Size 0 0) (Size 0 0)
  in
  { items = Dict.singleton 0 <| Item 0 (Topic rootTopic) Set.empty
  , boxes = Dict.singleton rootBoxId
    <| Box rootBoxId (Rectangle 0 0 0 0) (Point 0 0) Dict.empty
  , boxId = rootBoxId
  , nextId = 1
  ----- transient -----
  , imageCache = Dict.empty -- TODO: move to Text module, but should survive a map switch
  -- feature modules
  , text = Text.init
  , mouse = Mouse.init
  , search = Search.init
  , icon = Icon.init
  , selection = Sel.init
  }


initTransient : Model -> Model
initTransient model =
  { model
  -- feature modules
  | text = Text.init
  , mouse = Mouse.init
  , search = Search.init
  , icon = Icon.init
  , selection = Sel.init
  }


type Msg
  -- gestures detected by Mouse module
  = AddAssoc Id Id BoxId
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
    , ("boxes", model.boxes |> Dict.values |> E.list encodeBox)
    , ("boxId", E.int model.boxId)
    , ("nextId", E.int model.nextId)
    ]


decoder : D.Decoder Model
decoder =
  D.succeed Model
  |> required "items" (D.list itemDecoder |> D.andThen toDictDecoder)
  |> required "boxes" (D.list boxDecoder |> D.andThen toDictDecoder)
  |> required "boxId" D.int
  |> required "nextId" D.int
  ----- transient -----
  |> hardcoded Dict.empty
  -- feature modules
  |> hardcoded Text.init
  |> hardcoded Mouse.init
  |> hardcoded Search.init
  |> hardcoded Icon.init
  |> hardcoded Sel.init
