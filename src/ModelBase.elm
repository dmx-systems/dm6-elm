module ModelBase exposing (Id, TopicId(..), AssocId(..), ItemId(..), AssocIds,
  Topic, Icon, TextSize, Size, SizeField(..), Point, Rectangle, Assoc, AssocType(..), ItemSet,
  SetItem, Box, BoxId(..), Level, BoxPath, Target, BoxTarget, BoxTopic, Expansion(..), ImageId,
  Attrs, PointerType, toTopicId, toAssocId, toBoxId, fromBoxId, maybeTopicId, maybeAssocId,
  encodeBoxId, topicIdDecoder, boxIdDecoder)

import Renderer exposing (Renderer)

import Dict exposing (Dict)
import Html exposing (Attribute)
import Json.Decode as D
import Json.Encode as E



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


type alias Id = String


type TopicId
  = TopicId Id


type AssocId
  = AssocId Id


type ItemId
  = T TopicId
  | A AssocId


type BoxId
  = BoxId TopicId


toTopicId : TopicId -> Id
toTopicId (TopicId id) =
  id


toAssocId : AssocId -> Id
toAssocId (AssocId id) =
  id


toBoxId : BoxId -> Id
toBoxId (BoxId (TopicId id)) =
  id


fromBoxId : BoxId -> TopicId
fromBoxId (BoxId id) =
  id


maybeTopicId : ItemId -> Maybe TopicId
maybeTopicId id =
  case id of
    T id_ -> Just id_
    A _ -> Nothing


maybeAssocId : ItemId -> Maybe AssocId
maybeAssocId id =
  case id of
    T _ -> Nothing
    A id_ -> Just id_


type alias AssocIds = List AssocId
type alias Icon = String -- name of feather icon, https://feathericons.com
type alias ImageId = Id
type alias Attrs msg = List (Attribute msg)
type alias PointerType = String


-- Box

type alias Level = Int -- nesting level


type alias BoxPath = List BoxId


type alias Target = (ItemId, BoxPath)


type alias BoxTarget =
  { ixBoxPath : BoxPath
  , target : Target
  }


type alias Box =
  { id : BoxId
  , itemSetId : Id --> ItemSet
  , topics : Dict Id BoxTopic
  -- Note: no "assocs" here as assocs have no renderer-independent view properties at the moment
  , renderer : Renderer
  }


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



-- JSON


-- Encode

encodeBoxId : BoxId -> E.Value
encodeBoxId id =
  E.string (toBoxId id)


-- Decode

topicIdDecoder : D.Decoder TopicId
topicIdDecoder =
  D.map TopicId D.string


boxIdDecoder : D.Decoder BoxId
boxIdDecoder =
  D.map (BoxId << TopicId) D.string
