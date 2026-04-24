module ModelBase exposing (Id, Item, ItemInfo(..), TopicId(..), AssocId(..), ItemId(..),
  fromTopicId, fromAssocId, toId, AssocIds, TopicInfo, Icon, TextSize, Size, SizeField(..),
  Point, Rectangle, AssocInfo, AssocType(..), ItemSet, SetItem, Box, BoxId, BoxPath, Target,
  rootBoxId, BoxTopic, Expansion(..), ImageId, Attrs, PointerType, Extensions, ExtName,
  ExtLabel, Renderer, PosHint(..), ToolbarPos, encodeItem, encodeItemSet, encodeBox,
  itemDecoder, itemSetDecoder, boxDecoder, toDictDecoder)

import Dict exposing (Dict)
import Html exposing (Attribute)
import Json.Decode as D
import Json.Encode as E



-- TYPES


type alias IdRecord r =
  { r | id : Id }


-- Item

type alias Item =
  { id : Id
  , info : ItemInfo
  , assocIds : AssocIds -- The associations the item is involved in (as a "player")
                        -- TODO: move to TopicInfo?
  }


type ItemInfo
  = Topic TopicInfo
  | Assoc AssocInfo


type alias TopicInfo =
  { id : Id
  , icon : Maybe Icon
  , text : String
  , size : TextSize
  }


type alias AssocInfo =
  { id : Id
  , assocType : AssocType -- serialized as "type", field can't be named "type", a reserved word
  , player1 : Id
  , player2 : Id
  }


type AssocType
  = Hierarchy
  | Crosslink


type alias TextSize =
  { view: Size
  , editor: Size
  }


type SizeField
  = View
  | Editor


type alias Size =
  { w : Int
  , h : Int
  }


type alias Point =
  { x : Int
  , y : Int
  }


type alias Rectangle =
  { x1 : Int
  , y1 : Int
  , x2 : Int
  , y2 : Int
  }


type alias Id = Int


type TopicId
  = TopicId Id


type AssocId
  = AssocId Id


type ItemId
  = T TopicId
  | A AssocId


fromTopicId : Id -> ItemId
fromTopicId topicId =
  T <| TopicId topicId


fromAssocId : Id -> ItemId
fromAssocId assocId =
  A <| AssocId assocId


toId : ItemId -> Id
toId itemId =
  case itemId of
    T (TopicId id) -> id
    A (AssocId id) -> id


type alias AssocIds = List Id
type alias Icon = String -- name of feather icon, https://feathericons.com
type alias ImageId = Int
type alias Attrs msg = List (Attribute msg)
type alias PointerType = String


-- Box

type alias BoxId = Id
type alias BoxPath = List BoxId
type alias Target = (Id, BoxPath)


type alias Box =
  { id : BoxId
  , itemSetId : Id
  , topics : Dict Id BoxTopic
  -- Note: no "assocs" here as assocs have no renderer-independent view properties at the moment
  , renderer : Renderer
  }


rootBoxId : BoxId
rootBoxId = 0


{-| Attaches renderer-independent view properties to a SetItem.
At the moment this is just the Expansion state.
-}
type alias BoxTopic =
  { id : Id
  , expansion : Expansion
  }


type Expansion
  = Collapsed
  | Expanded


-- Item Set

type alias ItemSet =
  { id : Id -- item set ID
  , items : List SetItem
  }


type alias SetItem =
  { id : ItemId -- item ID
  -- TODO: add "dateAdded"
  }


-- Extensions

type alias ExtName = String
type alias ExtLabel = String
type alias Extensions = List (ExtName, ExtLabel)


type alias Renderer = String


type PosHint
  = Default
  | Random


type alias ToolbarPos =
  { topic : TopicInfo -> Point
  , assoc : AssocInfo -> Point
  }



-- JSON


-- Encode

encodeItem : Item -> E.Value
encodeItem item =
  E.object
    [ case item.info of
      Topic topic ->
        ( "topic"
        , E.object
          [ ("id", E.int topic.id)
          , ("icon", E.string <| Maybe.withDefault "" topic.icon)
          , ("text", E.string topic.text)
          , ("size", encodeTextSize topic.size)
          , ("assocIds", E.list E.int item.assocIds)
          ]
        )
      Assoc assoc ->
        ( "assoc"
        , E.object
          [ ("id", E.int assoc.id)
          , ("type", encodeAssocType assoc.assocType)
          , ("player1", E.int assoc.player1)
          , ("player2", E.int assoc.player2)
          , ("assocIds", E.list E.int item.assocIds)
          ]
        )
    ]


encodeTextSize : TextSize -> E.Value
encodeTextSize size =
  E.object
    [ ("view", E.object
        [ ("w", E.int size.view.w)
        , ("h", E.int size.view.h)
        ]
      )
    , ("editor", E.object
        [ ("w", E.int size.editor.w)
        , ("h", E.int size.editor.h)
        ]
      )
    ]


encodeAssocType : AssocType -> E.Value
encodeAssocType assocType =
  E.string <|
    case assocType of
      Hierarchy -> "Hierarchy"
      Crosslink -> "Crosslink"


encodeItemSet : ItemSet -> E.Value
encodeItemSet itemSet =
  E.object
    [ ("id", E.int itemSet.id)
    , ("items", E.list encodeSetItem itemSet.items)
    ]


encodeSetItem : SetItem -> E.Value
encodeSetItem setItem =
  E.object
    [ case setItem.id of
        T (TopicId id) -> ("topicId", E.int id)
        A (AssocId id) -> ("assocId", E.int id)
    ]


encodeBox : Box -> E.Value
encodeBox box =
  E.object
    [ ("id", E.int box.id)
    , ("itemSetId", E.int box.itemSetId)
    , ("topics", E.list encodeBoxTopic <| Dict.values box.topics)
    , ("renderer", E.string box.renderer)
    ]


encodeBoxTopic : BoxTopic -> E.Value
encodeBoxTopic topic =
  E.object
    [ ("id", E.int topic.id)
    , ("expansion", encodeExpansion topic.expansion)
    ]


encodeExpansion : Expansion -> E.Value
encodeExpansion expansion =
  E.string <|
    case expansion of
      Collapsed -> "Collapsed"
      Expanded -> "Expanded"


-- Decode

itemDecoder : D.Decoder Item
itemDecoder =
  D.oneOf
    [ D.field "topic"
      (D.map3 Item
        (D.field "id" D.int)
        (D.map Topic <| D.map4 TopicInfo
          (D.field "id" D.int)
          (D.field "icon" D.string |> D.andThen maybeString)
          (D.field "text" D.string)
          textSizeDecoder
        )
        assocIdsDecoder
      )
    , D.field "assoc"
      (D.map3 Item
        (D.field "id" D.int)
        (D.map Assoc <| D.map4 AssocInfo
          (D.field "id" D.int)
          (D.field "type" D.string |> D.andThen assocTypeDecoder)
          (D.field "player1" D.int)
          (D.field "player2" D.int)
        )
        assocIdsDecoder
      )
    ]


textSizeDecoder : D.Decoder TextSize
textSizeDecoder =
  (D.field "size" <| D.map2 TextSize
    (D.field "view" <| D.map2 Size
      (D.field "w" D.int)
      (D.field "h" D.int)
    )
    (D.field "editor" <| D.map2 Size
      (D.field "w" D.int)
      (D.field "h" D.int)
    )
  )


assocIdsDecoder : D.Decoder AssocIds
assocIdsDecoder =
  D.field "assocIds" (D.list D.int)


assocTypeDecoder : String -> D.Decoder AssocType
assocTypeDecoder str =
  case str of
    "Hierarchy" -> D.succeed Hierarchy
    "Crosslink" -> D.succeed Crosslink
    _ -> D.fail <| "\"" ++ str ++ "\" is an invalid AssocType"


itemSetDecoder : D.Decoder ItemSet
itemSetDecoder =
  D.map2 ItemSet
    (D.field "id" D.int)
    (D.field "items" <| D.list
      ( D.oneOf
        [ (D.field "topicId" D.int) |> D.map (SetItem << T << TopicId)
        , (D.field "assocId" D.int) |> D.map (SetItem << A << AssocId)
        ]
      )
    )


boxDecoder : D.Decoder Box
boxDecoder =
  D.map4 Box
    (D.field "id" D.int)
    (D.field "itemSetId" D.int)
    (D.field "topics" (boxTopicDecoder |> toDictDecoder))
    (D.field "renderer" D.string)


boxTopicDecoder : D.Decoder BoxTopic
boxTopicDecoder =
  D.map2 BoxTopic
    (D.field "id" D.int)
    (D.field "expansion" D.string |> D.andThen expansionDecoder)


expansionDecoder : String -> D.Decoder Expansion
expansionDecoder str =
  case str of
    "Collapsed" -> D.succeed Collapsed
    "Expanded" -> D.succeed Expanded
    _ -> D.fail ("\"" ++ str ++ "\" is an invalid Expansion")


toDictDecoder : D.Decoder (IdRecord r) -> D.Decoder (Dict Id (IdRecord r))
toDictDecoder =
  D.list >> D.map toDict


toDict : List (IdRecord r) -> Dict Id (IdRecord r)
toDict =
  List.map (\item -> (item.id, item)) >> Dict.fromList


maybeString : String -> D.Decoder (Maybe String)
maybeString str =
  D.succeed <|
    case str of
      "" -> Nothing
      _ -> Just str
