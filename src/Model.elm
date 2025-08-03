module Model exposing (..)

import Utils exposing (..)

import Dict exposing (Dict)
import Json.Decode as D
import Json.Decode.Pipeline exposing (required, hardcoded)
import Json.Encode as E
import String exposing (fromInt)
import Time



type alias Model =
  { items : Items
  , maps : Maps
  , activeMap : MapId
  , selection : Selection -- transient
  , editState : EditState -- transient
  , dragState : DragState -- transient
  , iconMenuState : Bool -- transient
  , measureText : String -- transient
  , nextId : Id
  }


defaultModel : Model
defaultModel =
  { items = Dict.empty
  , maps = Dict.singleton 0
    <| Map 0 Dict.empty (Rectangle 0 0 0 0) -1 -- parentMapId = -1
  , activeMap = 0
  , selection = []
  , editState = NoEdit
  , dragState = NoDrag
  , iconMenuState = False
  , measureText = ""
  , nextId = 1
  }


type alias Items = Dict Id Item


type Item -- TODO: make it a record with "id" field, analogue ViewItem?
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
type alias ViewItems = Dict Id ViewItem -- rename "MapItems"?


type alias Map =
  { id : MapId
  , items : ViewItems
  , rect : Rectangle
  , parentMapId : MapId
  }


type alias ViewItem = -- rename "MapItem"?
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
  | Set DisplayMode
  | Edit EditMsg
  | IconMenu IconMenuMsg
  | Mouse MouseMsg
  | Delete
  | NoOp


type EditMsg
  = ItemEditStart
  | ItemEditInput String
  | TextareaInput String
  | SetTopicSize Id MapId Size
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
    Just viewItem ->
      case viewItem.viewProps of
        ViewTopic props -> Just props
        ViewAssoc _ -> topicMismatch "getTopicProps" topicId Nothing
    Nothing -> fail "getTopicProps" {topicId = topicId, mapId = mapId} Nothing


updateTopicProps : Id -> MapId -> Maps -> (TopicProps -> TopicProps) -> Maps
updateTopicProps topicId mapId maps propsFunc =
  updateMaps
    mapId
    (\map ->
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
    )
    maps


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


viewAssocsOfPlayer : Id -> ViewItems -> Model -> List Id
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


isViewTopic : ViewItem -> Bool
isViewTopic item =
  case item.viewProps of
    ViewTopic _ -> True
    ViewAssoc _ -> False


isViewAssoc : ViewItem -> Bool
isViewAssoc item =
  not (isViewTopic item)


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
    [ selItem ] -> Just selItem
    _ -> Nothing



-- ENCODE/DECODE MODEL <-> JS VALUE (for storage)


encode : Model -> E.Value
encode model =
  E.object
    [ ("items", model.items |> E.dict
        String.fromInt
        encodeItem
      )
    , ("maps", model.maps |> E.dict
        String.fromInt
        encodeMap
      )
    , ("activeMap", E.int model.activeMap)
    , ("nextId", E.int model.nextId)
    ]


encodeItem : Item -> E.Value
encodeItem item =
  E.object
    [ case item of
      Topic topic ->
        ( "topic"
        , E.object
          [ ("id", E.int topic.id)
          , ("text", E.string topic.text)
          , ("iconName", E.string <| Maybe.withDefault "" topic.iconName)
          ]
        )
      Assoc assoc ->
        ( "assoc"
        , E.object
          [ ("id", E.int assoc.id)
          , ("itemType", E.string assoc.itemType)
          , ("player1", E.int assoc.player1)
          , ("role1", E.string assoc.role1)
          , ("player2", E.int assoc.player2)
          , ("role2", E.string assoc.role2)
          ]
        )
    ]


encodeMap : Map -> E.Value
encodeMap map =
  E.object
    [ ("id", E.int map.id)
    , ("items", map.items |> E.dict
        String.fromInt
        encodeMapItem
      )
    , ("rect", E.object
        [ ("x1", E.float map.rect.x1)
        , ("y1", E.float map.rect.y1)
        , ("x2", E.float map.rect.x2)
        , ("y2", E.float map.rect.y2)
        ]
      )
    , ("parentMapId", E.int map.parentMapId)
    ]


encodeMapItem : ViewItem -> E.Value
encodeMapItem item =
  E.object
    [ ("id", E.int item.id)
    , ("hidden", E.bool item.hidden)
    , case item.viewProps of
        ViewTopic topicProps ->
          ( "topicProps"
          , E.object
            [ ("pos", E.object
                [ ("x", E.float topicProps.pos.x)
                , ("y", E.float topicProps.pos.y)
                ]
              )
            , ("size", E.object
                [ ("w", E.float topicProps.size.w)
                , ("h", E.float topicProps.size.h)
                ]
              )
            , ("displayMode", encodeDisplayName topicProps.displayMode)
            ]
          )
        ViewAssoc assosProps ->
          ( "assocProps"
          , E.object []
          )
    , ("parentAssocId", E.int item.parentAssocId)
    ]


encodeDisplayName : DisplayMode -> E.Value
encodeDisplayName displayMode =
  E.string
    (case displayMode of
      Monad LabelOnly -> "LabelOnly"
      Monad Detail -> "Detail"
      Container BlackBox -> "BlackBox"
      Container WhiteBox -> "WhiteBox"
      Container Unboxed -> "Unboxed"
    )


decoder : D.Decoder Model
decoder =
  D.succeed Model
    |> required "items"
      (D.dict
        ( D.oneOf
          [ D.field "topic" <| D.map Topic <| D.map3 TopicInfo
              (D.field "id" D.int)
              (D.field "text" D.string)
              (D.field "iconName" D.string
                |> D.andThen maybeString
              )
          , D.field "assoc" <| D.map Assoc <| D.map6 AssocInfo
              (D.field "id" D.int)
              (D.field "itemType" D.string)
              (D.field "player1" D.int)
              (D.field "role1" D.string)
              (D.field "player2" D.int)
              (D.field "role2" D.string)
          ]
        ) |> D.andThen strToIntDictDecoder
      )
    |> required "maps" (D.dict mapDecoder |> D.andThen strToIntDictDecoder)
    |> required "activeMap" D.int
    |> hardcoded defaultModel.selection
    |> hardcoded defaultModel.editState
    |> hardcoded defaultModel.dragState
    |> hardcoded defaultModel.iconMenuState
    |> hardcoded defaultModel.measureText
    |> required "nextId" D.int


mapDecoder : D.Decoder Map
mapDecoder =
  D.map4 Map
    (D.field "id" D.int)
    (D.field "items"
      (D.dict
        (D.map4 ViewItem
          (D.field "id" D.int)
          (D.field "hidden" D.bool)
          (D.oneOf
            [ D.field "topicProps" <| D.map ViewTopic <| D.map3 TopicProps
              (D.field "pos" <| D.map2 Point
                (D.field "x" D.float)
                (D.field "y" D.float)
              )
              (D.field "size" <| D.map2 Size
                (D.field "w" D.float)
                (D.field "h" D.float)
              )
              (D.field "displayMode" D.string |> D.andThen displayModeDecoder)
            , D.field "assocProps" <| D.succeed (ViewAssoc AssocProps)
            ]
          )
          (D.field "parentAssocId" D.int)
        ) |> D.andThen strToIntDictDecoder
      )
    )
    (D.field "rect" <| D.map4 Rectangle
      (D.field "x1" D.float)
      (D.field "y1" D.float)
      (D.field "x2" D.float)
      (D.field "y2" D.float)
    )
    (D.field "parentMapId" D.int)


strToIntDictDecoder : Dict String v -> D.Decoder (Dict Id v)
strToIntDictDecoder strDict =
  case strToIntDict strDict of
    Just dict -> D.succeed dict
    Nothing -> D.fail "Transformation Dict String -> Int failed"


strToIntDict : Dict String v -> Maybe (Dict Id v)
strToIntDict strDict =
  strDict |> Dict.foldl
    (\k v b ->
      case b of
        Just b_ ->
          case String.toInt k of
            Just i -> Just (Dict.insert i v b_)
            Nothing -> Nothing
        Nothing -> Nothing
    )
    (Just Dict.empty)


maybeString : String -> D.Decoder (Maybe String)
maybeString str =
  D.succeed
    ( case str of
        "" -> Nothing
        _ -> Just str
    )


displayModeDecoder : String -> D.Decoder DisplayMode
displayModeDecoder str =
  case str of
    "LabelOnly" -> D.succeed (Monad LabelOnly)
    "Detail" -> D.succeed (Monad Detail)
    "BlackBox" -> D.succeed (Container BlackBox)
    "WhiteBox" -> D.succeed (Container WhiteBox)
    "Unboxed" -> D.succeed (Container Unboxed)
    _ -> D.fail <| "\"" ++ str ++ "\" is an invalid display mode"



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
