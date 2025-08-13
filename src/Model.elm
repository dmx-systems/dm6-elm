module Model exposing (..)

import Utils exposing (..)

import Dict exposing (Dict)
import String exposing (fromInt)
import Time



type alias Model =
  { items : Items
  , maps : Maps
  , mapPath : List MapId
  , nextId : Id
  , selection : Selection -- transient
  , editState : EditState -- transient
  , dragState : DragState -- transient
  , iconMenuState : Bool -- transient
  , measureText : String -- transient
  }


defaultModel : Model
defaultModel =
  { items = Dict.empty
  , maps = Dict.singleton 0 -- map 0 is the "home map", it has no corresponding topic
    <| Map 0 Dict.empty (Rectangle 0 0 0 0) -1 -- parentMapId = -1
  , mapPath = [0]
  , nextId = 1
  , selection = []
  , editState = NoEdit
  , dragState = NoDrag
  , iconMenuState = False
  , measureText = ""
  }


type alias Items = Dict Id Item


type Item -- TODO: make it a record with "id" field, analogue MapItem?
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
type alias MapItems = Dict Id MapItem


type alias Map =
  { id : MapId
  , items : MapItems
  , rect : Rectangle
  , parentMapId : MapId -- FIXME: ambiguous semantics? view context vs model?
  }


type alias MapItem =
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
  , size : Size
  , displayMode : DisplayMode
  }


type alias AssocProps =
  {}


type DisplayMode
  = Monad MonadDisplay
  | Container ContainerDisplay


type MonadDisplay
  = LabelOnly
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


type alias Selection = List (Id, MapId)


type alias Id = Int
type alias MapId = Id
type alias Class = String -- a CSS class, e.g. "dmx-topic"
type alias ItemType = String -- a type URI, e.g. "dmx.association"
type alias RoleType = String -- a role type URI, e.g. "dmx.default"
type alias Delta = Point
type alias IconName = String -- name of feather icon, https://feathericons.com


type EditState
  = ItemEdit Id MapId
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
  | SwitchDisplay DisplayMode
  | Edit EditMsg
  | IconMenu IconMenuMsg
  | Mouse MouseMsg
  | Nav NavMsg
  | Delete
  | NoOp


type EditMsg
  = EditStart
  | OnTextInput String
  | OnTextareaInput String
  | SetTopicSize Id MapId Size
  | EditEnd


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


type NavMsg
  = Fullscreen
  | Back



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


getTopicLabel : TopicInfo -> String
getTopicLabel topic =
  case topic.text |> String.lines |> List.head of
    Just line -> line
    Nothing -> ""


-- Maps


isHome : Model -> Bool
isHome model =
  activeMap model == 0


isFullscreen : MapId -> Model -> Bool
isFullscreen mapId model =
  activeMap model == mapId


activeMap : Model -> MapId
activeMap model =
  case List.head model.mapPath of
    Just mapId -> mapId
    Nothing -> logError "activeMap" "mapPath is empty!" 0


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


updateMapRect : MapId -> (Rectangle -> Rectangle) -> Model -> Model
updateMapRect mapId rectFunc model =
  { model | maps = updateMaps
    mapId
    (\map ->
      { map | rect = rectFunc map.rect }
    )
    model.maps
  }


getTopicPos : Id -> MapId -> Maps -> Maybe Point
getTopicPos topicId mapId maps =
  case getTopicProps topicId mapId maps of
    Just { pos } -> Just pos
    Nothing -> fail "getTopicPos" {topicId = topicId, mapId = mapId} Nothing


setTopicPos : Id -> MapId -> Point -> Maps -> Maps
setTopicPos topicId mapId pos maps =
  updateTopicProps topicId mapId maps
    (\props -> { props | pos = pos })


setTopicPosByDelta : Id -> MapId -> Delta -> Maps -> Maps
setTopicPosByDelta topicId mapId delta maps =
  updateTopicProps topicId mapId maps
    (\props ->
      { props | pos =
        Point
          (props.pos.x + delta.x)
          (props.pos.y + delta.y)
      }
    )


getTopicSize : Id -> MapId -> Maps -> Maybe Size
getTopicSize topicId mapId maps =
  case getTopicProps topicId mapId maps of
    Just { size } -> Just size
    Nothing -> fail "getTopicSize" {topicId = topicId, mapId = mapId} Nothing


setTopicSize : Id -> MapId -> Size -> Maps -> Maps
setTopicSize topicId mapId size maps =
  updateTopicProps topicId mapId maps
    (\props -> { props | size = size })


getDisplayMode : Id -> MapId -> Maps -> Maybe DisplayMode
getDisplayMode topicId mapId maps =
  case getTopicProps topicId mapId maps of
    Just { displayMode } -> Just displayMode
    Nothing -> fail "getDisplayMode" {topicId = topicId, mapId = mapId} Nothing


setDisplayMode : Id -> MapId -> DisplayMode -> Model -> Maps
setDisplayMode topicId mapId displayMode model =
  updateTopicProps topicId mapId model.maps
    (\props -> { props | displayMode = displayMode })


getTopicProps : Id -> MapId -> Maps -> Maybe TopicProps
getTopicProps topicId mapId maps =
  case getViewItemById topicId mapId maps of
    Just mapItem ->
      case mapItem.viewProps of
        ViewTopic props -> Just props
        ViewAssoc _ -> topicMismatch "getTopicProps" topicId Nothing
    Nothing -> fail "getTopicProps" {topicId = topicId, mapId = mapId} Nothing


updateTopicProps : Id -> MapId -> Maps -> (TopicProps -> TopicProps) -> Maps
updateTopicProps topicId mapId maps propsFunc =
  maps |> updateMaps mapId
    (\map ->
      { map | items = map.items |> Dict.update topicId
        (\mapItem_ ->
          case mapItem_ of
            Just mapItem ->
              case mapItem.viewProps of
                ViewTopic props -> Just
                  { mapItem | viewProps = ViewTopic (propsFunc props) }
                ViewAssoc _ -> topicMismatch "updateTopicProps" topicId Nothing
            Nothing -> illegalItemId "updateTopicProps" topicId Nothing
        )
      }
    )


getViewItemById : Id -> MapId -> Maps -> Maybe MapItem
getViewItemById itemId mapId maps =
  getMap mapId maps |> Maybe.andThen (getViewItem itemId)


getViewItem : Id -> Map -> Maybe MapItem
getViewItem itemId map =
  case map.items |> Dict.get itemId of
    Just mapItem -> Just mapItem
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


hideItems : Id -> Model -> MapItems -> MapItems
hideItems itemId model items =
  let
    newItems = items |> Dict.update
      itemId
      (\item_ ->
        case item_ of
          Just item -> Just { item | hidden = True }
          Nothing -> Nothing
      )
    assocIds = viewAssocsOfPlayer itemId items model
  in
  List.foldr
    (\assocId newItems_ -> hideItems assocId model newItems_)
    newItems
    assocIds


deleteItem : Id -> Model -> Model
deleteItem itemId model =
  assocsOfPlayer itemId model |> List.foldr
    deleteItem -- recursion
    { model
      | items = model.items |> Dict.remove itemId -- delete item
      , maps = model.maps |> Dict.map -- delete item from all maps
        (\_ map -> { map | items = map.items |> Dict.remove itemId })
    }


assocsOfPlayer : Id -> Model -> List Id
assocsOfPlayer playerId model =
  model.items |> Dict.values
    |> List.filter isAssoc
    |> List.map getItemId
    |> List.filter (hasPlayer playerId model)


viewAssocsOfPlayer : Id -> MapItems -> Model -> List Id
viewAssocsOfPlayer playerId items model =
  items |> Dict.values
    |> List.filter isViewAssoc
    |> List.map .id
    |> List.filter (hasPlayer playerId model)


hasPlayer : Id -> Model -> Id -> Bool
hasPlayer playerId model assocId =
  case getAssocInfo assocId model of
    Just assoc -> assoc.player1 == playerId || assoc.player2 == playerId
    Nothing -> False


getItemId : Item -> Id
getItemId item =
  case item of
    Topic {id} -> id
    Assoc {id} -> id


isTopic : Item -> Bool
isTopic item =
  case item of
    Topic _ -> True
    Assoc _ -> False


isAssoc : Item -> Bool
isAssoc item =
  not (isTopic item)


isViewTopic : MapItem -> Bool
isViewTopic item =
  case item.viewProps of
    ViewTopic _ -> True
    ViewAssoc _ -> False


isViewAssoc : MapItem -> Bool
isViewAssoc item =
  not (isViewTopic item)


isVisible : MapItem -> Bool
isVisible item =
  not item.hidden


-- Selection

select : Id -> MapId -> Model -> Model
select id mapId model =
  { model | selection = [ (id, mapId) ] }


getSingleSelection : Model -> Maybe (Id, MapId)
getSingleSelection model =
  case model.selection of
    [ selItem ] -> Just selItem
    _ -> Nothing



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
