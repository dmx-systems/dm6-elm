module Model exposing (..)

import Dict exposing (Dict)
import String exposing (fromInt)
import Time
import Debug exposing (log, toString)

-- TODO: move to "Utils"
import Html exposing (Attribute)
import Html.Events exposing (stopPropagationOn)
import Json.Decode as D



type alias Model =
  { items : Items
  , maps : Maps
  , activeMap : MapId
  , selection : Selection -- transient
  , dragState : DragState -- transient
  , isEditDialogOpen : Bool -- transient
  , nextId : Id
  }


type alias Items = Dict Id Item


type Item
  = Topic TopicInfo
  | Assoc AssocInfo


type alias TopicInfo =
  { id : Id
  , color : Color
  , iconName : Maybe IconName
  }


type alias AssocInfo =
  { id : Id
  , itemType : ItemType
  , player1 : Id
  , role1 : RoleType
  , player2 : Id
  , role2 : RoleType
  }


type alias Maps = Dict Id Map
type alias ViewItems = Dict Id ViewItem


type alias TransferFunc = ViewItems -> ViewItems -> Model -> ViewItems


type alias Map =
  { id : MapId
  , items : ViewItems
  , rect : Rectangle
  , parentMapId : MapId
  }


type alias ViewItem =
  { id : Id
  , hidden : Bool
  , viewProps : ViewProps
  , parentAssocId : Id
  }


type ViewProps
  = ViewTopic TopicProps
  | ViewAssoc AssocProps


type alias TopicProps =
  { pos : Point
  , displayMode : Maybe DisplayMode -- only set for container topics
  }


type alias AssocProps =
  {}


type DisplayMode -- TODO: rename to "ContainerDisplay"?
  = BlackBox
  | WhiteBox
  | Unboxed


type alias Point =
  { x : Float
  , y : Float
  }


type alias Rectangle =
  { x1 : Float
  , y1 : Float
  , x2 : Float
  , y2 : Float
  }


-- not used
type alias Size =
  { width : Float
  , height : Float
  }


type alias Id = Int
type alias MapId = Id
type alias Class = String -- a CSS class, e.g. "dmx-topic"
type alias ItemType = String -- a type URI, e.g. "dmx.association"
type alias RoleType = String -- a role type URI, e.g. "dmx.default"
type alias Selection = List (Id, MapId)
type alias Delta = Point
type alias Color = Int -- Hue
type alias IconName = String -- name of feather icon, https://feathericons.com


type DragState
  = NoDrag
  | WaitForStartTime Class Id MapId Point -- start point (mouse)
  | DragEngaged Time.Posix Class Id MapId Point -- start point (mouse)
  | WaitForEndTime Time.Posix Class Id MapId Point -- start point (mouse)
  | Drag DragMode Id MapId Point Point (Maybe (Id, MapId)) -- orig topic pos, last point (mouse)


type DragMode
  = DragTopic
  | DrawAssoc


type Msg
  = AddTopic
  | MoveTopicToMap Id MapId Point Id MapId Point -- start point, random point (for target)
  | Set (Maybe DisplayMode)
  | Edit EditMsg
  | Mouse MouseMsg
  | Delete
  | NoOp


type EditMsg
  = Open
  | Close
  | SetIcon (Maybe IconName)


type MouseMsg
  = Down -- mouse down somewhere
  | DownItem Class Id MapId Point -- mouse down on an item where a drag can be engaged
  | Move Point
  | Up
  | Over Class Id MapId
  | Out Class Id MapId
  | Time Time.Posix



-- MODEL HELPER


-- Items

getTopicInfo : Id -> Model -> Maybe TopicInfo
getTopicInfo topicId model =
  case model.items |> Dict.get topicId of
    Just item ->
      case item of
        Topic topic -> Just topic
        Assoc _ -> topicMismatch "getTopicInfo" topicId Nothing
    Nothing -> illegalItemId "getTopicInfo" topicId Nothing


getAssocInfo : Id -> Model -> Maybe AssocInfo
getAssocInfo assocId model =
  case model.items |> Dict.get assocId of
    Just item ->
      case item of
        Topic _ -> assocMismatch "getAssocInfo" assocId Nothing
        Assoc assoc -> Just assoc
    Nothing -> illegalItemId "getAssocInfo" assocId Nothing


updateTopicInfo : Id -> (TopicInfo -> TopicInfo) -> Model -> Model
updateTopicInfo topicId topicFunc model =
  { model | items = model.items |> Dict.update topicId
    (\maybeItem ->
      case maybeItem of
        Just item -> case item of
          Topic topic -> Just (topicFunc topic |> Topic)
          Assoc _  -> topicMismatch "updateTopicInfo" topicId Nothing
        Nothing -> illegalItemId "updateTopicInfo" topicId Nothing
    )
  }


-- Selection

select : Id -> MapId -> Model -> Model
select id mapId model =
  { model | selection = [ (id, mapId) ] }


getSingleSelection : Model -> Maybe (Id, MapId)
getSingleSelection model =
  case model.selection of
    [] -> Nothing
    selItem :: selItems -> Just selItem
    -- FIXME: return nothing if more than one item



-- EVENT HELPER -- TODO: move to "Utils"


stopPropagationOnMousedown : Attribute Msg
stopPropagationOnMousedown =
  stopPropagationOn "mousedown" <| D.succeed (NoOp, True)



-- DEBUG


itemNotInMap : String -> Id -> Id -> a -> a
itemNotInMap funcName itemId mapId val =
  logError funcName ("item " ++ fromInt itemId ++ " not in map " ++ fromInt mapId) val


topicMismatch : String -> Id -> a -> a
topicMismatch funcName id val =
  logError funcName (fromInt id ++ " is not a Topic but an Assoc") val


assocMismatch : String -> Id -> a -> a
assocMismatch funcName id val =
  logError funcName (fromInt id ++ " is not an Assoc but a Topic") val


illegalMapId : String -> Id -> a -> a
illegalMapId funcName id val =
  illegalId funcName "Map" id val


illegalItemId : String -> Id -> a -> a
illegalItemId funcName id val =
  illegalId funcName "Item" id val


illegalId : String -> String -> Id -> a -> a
illegalId funcName item id val =
  logError funcName (fromInt id ++ " is an illegal " ++ item ++ " ID") val

--

logError : String -> String -> v -> v
logError funcName text val =
  log ("### ERROR @" ++ funcName ++ ": " ++ text) val


fail : String -> a -> v -> v
fail funcName args val =
  log ("--> @" ++ funcName ++ " failed " ++ toString args) val


call : String -> a -> v -> v
call funcName args val =
  log ("@" ++ funcName ++ " " ++ toString args ++ " -->") val


info : String -> v -> v
info funcName val =
  log ("@" ++ funcName) val
