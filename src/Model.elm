module Model exposing (..)

import Dict exposing (Dict)
import String exposing (fromInt)
import Time
import Debug exposing (log, toString)

-- TODO: move to "Utils"
import Html exposing (Attribute)
import Html.Events exposing (on, stopPropagationOn, keyCode)
import Json.Decode as D



type alias Model =
  { items : Items
  , maps : Maps
  , activeMap : MapId
  , selection : Selection -- transient
  , editState : EditState -- transient
  , dragState : DragState -- transient
  , iconMenuState : Bool -- transient
  , nextId : Id
  }


type alias Items = Dict Id Item


type Item
  = Topic TopicInfo
  | Assoc AssocInfo


type alias TopicInfo =
  { id : Id
  , text : String
  , iconName : Maybe IconName
  }


type alias AssocInfo =
  { id : Id
  , itemType : ItemType -- can't be named "type", a reserved word
  , player1 : Id
  , role1 : RoleType
  , player2 : Id
  , role2 : RoleType
  }


type alias Maps = Dict Id Map
type alias ViewItems = Dict Id ViewItem


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
  , displayMode : DisplayMode
  }


type alias AssocProps =
  {}


type DisplayMode
  = Monad MonadDisplay
  | Container ContainerDisplay


type MonadDisplay
  = Label
  | Detail


type ContainerDisplay
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


type alias Size =
  { w : Float
  , h : Float
  }


type alias Id = Int
type alias MapId = Id
type alias Class = String -- a CSS class, e.g. "dmx-topic"
type alias ItemType = String -- a type URI, e.g. "dmx.association"
type alias RoleType = String -- a role type URI, e.g. "dmx.default"
type alias Selection = List (Id, MapId)
type alias Delta = Point
type alias IconName = String -- name of feather icon, https://feathericons.com


type EditState
  = ItemEdit Id
  | NoEdit


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
  | Set DisplayMode
  | Edit EditMsg
  | IconMenu IconMenuMsg
  | Mouse MouseMsg
  | Delete
  | NoOp


type EditMsg
  = ItemEditStart
  | ItemEditInput String
  | ItemEditBlur String
  | ItemEditEnd


type IconMenuMsg
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
        Just item ->
          case item of
            Topic topic -> Just (topicFunc topic |> Topic)
            Assoc _  -> topicMismatch "updateTopicInfo" topicId Nothing
        Nothing -> illegalItemId "updateTopicInfo" topicId Nothing
    )
  }


-- Maps

getMap : MapId -> Maps -> Maybe Map
getMap mapId maps =
  case getMapIfExists mapId maps of
    Just map -> Just map
    Nothing -> illegalMapId "getMap" mapId Nothing


getMapIfExists : MapId -> Maps -> Maybe Map
getMapIfExists mapId maps =
  maps |> Dict.get mapId


hasMap : MapId -> Maps -> Bool
hasMap mapId maps =
  maps |> Dict.member mapId


getTopicPos : Id -> MapId -> Maps -> Maybe Point
getTopicPos topicId mapId maps =
  case getTopicProps topicId mapId maps of
    Just { pos } -> Just pos
    Nothing -> fail "getTopicPos" {topicId = topicId, mapId = mapId} Nothing


updateTopicPos : Id -> MapId -> Delta -> Maps -> Maps
updateTopicPos topicId mapId delta maps =
  updateTopicProps topicId mapId maps
    (\props ->
      { props | pos =
        Point
          (props.pos.x + delta.x)
          (props.pos.y + delta.y)
      }
    )


getTopicProps : Id -> MapId -> Maps -> Maybe TopicProps
getTopicProps topicId mapId maps =
  case getViewItemById topicId mapId maps of
    Just viewItem ->
      case viewItem.viewProps of
        ViewTopic props -> Just props
        ViewAssoc _ -> topicMismatch "getTopicProps" topicId Nothing
    Nothing -> fail "getTopicProps" {topicId = topicId, mapId = mapId} Nothing


updateTopicProps : Id -> MapId -> Maps -> (TopicProps -> TopicProps) -> Maps
updateTopicProps topicId mapId maps propsFunc =
  updateMaps
    mapId
    (updateTopicProps_ topicId propsFunc)
    maps


updateTopicProps_ : Id -> (TopicProps -> TopicProps) -> Map -> Map
updateTopicProps_ topicId propsFunc map =
  { map | items = map.items |> Dict.update topicId
    (\viewItem_ ->
      case viewItem_ of
        Just viewItem ->
          case viewItem.viewProps of
            ViewTopic props -> Just
              { viewItem | viewProps = ViewTopic (propsFunc props) }
            ViewAssoc _ -> topicMismatch "updateTopicProps" topicId Nothing
        Nothing -> illegalItemId "updateTopicProps" topicId Nothing
    )
  }


getViewItemById : Id -> MapId -> Maps -> Maybe ViewItem
getViewItemById itemId mapId maps =
  getMap mapId maps |> Maybe.andThen (getViewItem itemId)


getViewItem : Id -> Map -> Maybe ViewItem
getViewItem itemId map =
  case map.items |> Dict.get itemId of
    Just viewItem -> Just viewItem
    Nothing -> itemNotInMap "getViewItem" itemId map.id Nothing


updateMaps : MapId -> (Map -> Map) -> Maps -> Maps
updateMaps mapId mapFunc maps =
  maps |> Dict.update mapId
    (\map_ ->
      case map_ of
        Just map -> Just (mapFunc map)
        Nothing -> illegalMapId "updateMaps" mapId Nothing
    )


hideItem : Id -> MapId -> Maps -> Model -> Maps
hideItem itemId mapId maps model =
  updateMaps
    mapId
    (\map -> { map | items = hideItems itemId model map.items })
    maps


hideItems : Id -> Model -> ViewItems -> ViewItems
hideItems itemId model items =
  let
    newItems = items |> Dict.update
      itemId
      (\item_ ->
        case item_ of
          Just item -> Just { item | hidden = True }
          Nothing -> Nothing
      )
    assocIds = assocsOfPlayer itemId items model
  in
  List.foldr
    (\assocId newItems_ -> hideItems assocId model newItems_)
    newItems
    assocIds


assocsOfPlayer : Id -> ViewItems -> Model -> List Id
assocsOfPlayer itemId items model =
  items |> Dict.values
    |> List.filter isAssoc
    |> List.filter (hasPlayer itemId model)
    |> List.map .id


hasPlayer : Id -> Model -> ViewItem -> Bool
hasPlayer itemId model assocItem =
  case getAssocInfo assocItem.id model of
    Just assoc -> assoc.player1 == itemId || assoc.player2 == itemId
    Nothing -> False


isTopic : ViewItem -> Bool
isTopic item =
  case item.viewProps of
    ViewTopic _ -> True
    ViewAssoc _ -> False


isAssoc : ViewItem -> Bool
isAssoc item =
  case item.viewProps of
    ViewTopic _ -> False
    ViewAssoc _ -> True


isVisible : ViewItem -> Bool
isVisible item =
  not item.hidden


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


onEnterOrEsc : Msg -> Attribute Msg
onEnterOrEsc msg =
  on "keydown"
    ( D.oneOf
        [ keyDecoder 13 msg
        , keyDecoder 27 msg
        ]
    )


keyDecoder : Int -> Msg -> D.Decoder Msg
keyDecoder key msg =
  let
    isKey code =
      if code == key then
        D.succeed msg
      else
        D.fail "not that key"
  in
  keyCode |> D.andThen isKey


onBlur : (String -> Msg) -> Attribute Msg
onBlur msg =
  on "blur"
    ( (D.at ["target", "innerText"] D.string)
      |> D.andThen (\text -> D.succeed (msg text))
    )


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
