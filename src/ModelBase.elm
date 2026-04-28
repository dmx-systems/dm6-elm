module ModelBase exposing (Id, TopicId(..), AssocId(..), ItemId(..), AssocIds, Topic, Icon,
  TextSize, Size, SizeField(..), Point, Rectangle, Assoc, AssocType(..), ItemSet, SetItem, Box,
  BoxId, BoxPath, Target, BoxTopic, Expansion(..), ImageId, Attrs, PointerType, Extensions,
  ExtLabel, PosHint(..), ToolbarPos, fromTopicId, fromAssocId, toId, toTopicId, toAssocId,
  rootBoxId, encodeTopic, encodeAssoc, encodeItemSet, encodeBox, topicDecoder, assocDecoder,
  itemSetDecoder, boxDecoder, topicIdDecoder, toDictDecoder, toDictDecoderWith)

import Extension exposing (Renderer, encodeRenderer)

import Dict exposing (Dict)
import Html exposing (Attribute)
import Json.Decode as D
import Json.Encode as E



-- TYPES


type alias Entity e i =
  { e | id : i }


-- Item

type alias Topic =
  { id : TopicId
  , icon : Maybe Icon
  , text : String
  , size : TextSize
  , assocIds : AssocIds -- The associations connecting this topic
  }


type alias Assoc =
  { id : AssocId
  , assocType : AssocType -- serialized as "type", field can't be named "type", a reserved word
  , topicId1 : TopicId
  , topicId2 : TopicId
  }


type AssocType
  = Association
  | Hierarchy


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
    T id -> toTopicId id
    A id -> toAssocId id


toTopicId : TopicId -> Id
toTopicId (TopicId id) =
  id


toAssocId : AssocId -> Id
toAssocId (AssocId id) =
  id


type alias AssocIds = List AssocId
type alias Icon = String -- name of feather icon, https://feathericons.com
type alias ImageId = Int
type alias Attrs msg = List (Attribute msg)
type alias PointerType = String


-- Box

type alias BoxId = Id
type alias BoxPath = List BoxId
type alias Target = (ItemId, BoxPath)


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
  { id : TopicId
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
  { id : ItemId
  -- TODO: add "dateAdded"
  }


-- Extensions

type alias ExtName = String
type alias ExtLabel = String
type alias Extensions = List (ExtName, ExtLabel)


type PosHint
  = Default
  | Random


type alias ToolbarPos =
  { topic : Topic -> Point
  , assoc : Assoc -> Point
  }



-- JSON


-- Encode

encodeTopic : Topic -> E.Value
encodeTopic {id, icon, text, size, assocIds} =
  E.object
    [ ("id", E.int (toTopicId id))
    , ("icon", E.string <| Maybe.withDefault "" icon)
    , ("text", E.string text)
    , ("size", encodeTextSize size)
    , ("assocIds", E.list (toAssocId >> E.int) assocIds)
    ]


encodeAssoc : Assoc -> E.Value
encodeAssoc {id, assocType, topicId1, topicId2} =
  E.object
    [ ("id", E.int (toAssocId id))
    , ("type", encodeAssocType assocType)
    , ("topicId1", E.int (toTopicId topicId1))
    , ("topicId2", E.int (toTopicId topicId2))
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
      Association -> "Association"
      Hierarchy -> "Hierarchy"


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
    , ("renderer", encodeRenderer box.renderer)
    ]


encodeBoxTopic : BoxTopic -> E.Value
encodeBoxTopic topic =
  E.object
    [ ("id", E.int (toTopicId topic.id))
    , ("expansion", encodeExpansion topic.expansion)
    ]


encodeExpansion : Expansion -> E.Value
encodeExpansion expansion =
  E.string <|
    case expansion of
      Collapsed -> "Collapsed"
      Expanded -> "Expanded"


-- Decode

topicDecoder : D.Decoder Topic
topicDecoder =
  D.map5 Topic
    (D.field "id" topicIdDecoder)
    (D.field "icon" D.string |> D.map maybeString)
    (D.field "text" D.string)
    textSizeDecoder
    assocIdsDecoder


assocDecoder : D.Decoder Assoc
assocDecoder =
  (D.map4 Assoc
    (D.field "id" assocIdDecoder)
    (D.field "type" D.string |> D.andThen assocTypeDecoder)
    (D.field "topicId1" topicIdDecoder)
    (D.field "topicId2" topicIdDecoder)
  )


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
  D.field "assocIds" (D.list assocIdDecoder)


assocTypeDecoder : String -> D.Decoder AssocType
assocTypeDecoder str =
  case str of
    "Association" -> D.succeed Association
    "Hierarchy" -> D.succeed Hierarchy
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
    (D.field "topics" (toDictDecoderWith toTopicId boxTopicDecoder))
    (D.field "renderer" (Extension.rendererDecoder D.string))


boxTopicDecoder : D.Decoder BoxTopic
boxTopicDecoder =
  D.map2 BoxTopic
    (D.field "id" topicIdDecoder)
    (D.field "expansion" D.string |> D.andThen expansionDecoder)


expansionDecoder : String -> D.Decoder Expansion
expansionDecoder str =
  case str of
    "Collapsed" -> D.succeed Collapsed
    "Expanded" -> D.succeed Expanded
    _ -> D.fail ("\"" ++ str ++ "\" is an invalid Expansion")


toDictDecoder : D.Decoder (Entity e Id) -> D.Decoder (Dict Id (Entity e Id))
toDictDecoder entityDecoder =
  toDictDecoderWith identity entityDecoder


{-| A Dict decoder that takes an extra function to unwrap the entity ID so it can be used as a
Dict key.
-}
toDictDecoderWith : (i -> Id) -> D.Decoder (Entity e i) -> D.Decoder (Dict Id (Entity e i))
toDictDecoderWith unwrapId entityDecoder =
  D.list entityDecoder |> D.map
    (\items ->
      items
        |> List.map (\item -> (unwrapId item.id, item))
        |> Dict.fromList
    )


topicIdDecoder : D.Decoder TopicId
topicIdDecoder =
  D.int |> D.map TopicId


assocIdDecoder : D.Decoder AssocId
assocIdDecoder =
  D.int |> D.map AssocId


maybeString : String -> Maybe String
maybeString str =
  case str of
    "" -> Nothing
    _ -> Just str
