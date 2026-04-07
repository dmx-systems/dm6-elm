module ModelBase exposing (Id, Item, Items, ItemInfo(..), AssocIds, TopicInfo, Icon, TextSize,
  Size, SizeField(..), Point, Rectangle, AssocInfo, AssocType(..), ItemSet, ItemSets, SetItem,
  Box, Boxes, BoxId, BoxPath, homeBoxId, ItemProps, DisplayMode(..), TopicDisplay(..),
  BoxDisplay(..), ImageId, Attrs, PointerType, encodeItem, encodeItemSet, encodeBox,
  encodeDisplayMode, itemDecoder, itemSetDecoder, boxDecoder, toDictDecoder)

import BoxRendererDef exposing (Renderer)

import Dict exposing (Dict)
import Html exposing (Attribute)
import Json.Decode as D
import Json.Encode as E
import Set exposing (Set)



-- TYPES


type alias IdRecord r =
  { r | id : Id }


-- Item

type alias Items = Dict Id Item


type alias Item =
  { id : Id
  , info : ItemInfo
  , assocIds : AssocIds -- The associations the item is involved in (as a "player")
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
type alias AssocIds = Set Id
type alias Icon = String -- name of feather icon, https://feathericons.com
type alias ImageId = Int
type alias Attrs msg = List (Attribute msg)
type alias PointerType = String


-- Box

type alias BoxId = Id
type alias BoxPath = List BoxId


type alias Boxes = Dict BoxId Box


type alias Box =
  { id : BoxId
  , itemSetId : Id
  , itemProps : Dict Id ItemProps
  , renderer : Renderer
  }


-- TODO: differentiate topic/assoc
type alias ItemProps =
  { id : Id
  , displayMode : DisplayMode -- serialized as "display", TODO: rename to "display"?
  }


homeBoxId : BoxId
homeBoxId = 0


-- Item Set

type alias ItemSets = Dict Id ItemSet


type alias ItemSet =
  { id : Id -- set ID
  , items : List SetItem
  }


type alias SetItem =
  { id : Id -- item ID
  , boxAssocId : Id -- the Hierarchy association which connects the item with the box
  -- TODO: add "dateAdded"
  }


-- Display Mode

-- TODO: unify TopicDisplay/BoxDisplay -> Expanded/Minimized
type DisplayMode
  = TopicD TopicDisplay
  | BoxD BoxDisplay


type TopicDisplay
  = LabelOnly
  | Detail


type BoxDisplay
  = BlackBox
  | WhiteBox
  | Unboxed -- TODO: drop it, introduce Tree renderer instead



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
          , ("assocIds", E.set E.int item.assocIds)
          ]
        )
      Assoc assoc ->
        ( "assoc"
        , E.object
          [ ("id", E.int assoc.id)
          , ("type", encodeAssocType assoc.assocType)
          , ("player1", E.int assoc.player1)
          , ("player2", E.int assoc.player2)
          , ("assocIds", E.set E.int item.assocIds)
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
    [ ("id", E.int setItem.id)
    , ("boxAssocId", E.int setItem.boxAssocId)
    ]


encodeBox : Box -> E.Value
encodeBox box =
  E.object
    [ ("id", E.int box.id)
    , ("itemSetId", E.int box.itemSetId)
    , ("itemProps", E.list encodeItemProps <| Dict.values box.itemProps)
    , ("renderer", BoxRendererDef.encode box.renderer)
    ]


encodeItemProps : ItemProps -> E.Value
encodeItemProps itemProps =
  E.object
    [ ("id", E.int itemProps.id)
    , ("display", encodeDisplayMode itemProps.displayMode)
    ]


encodeDisplayMode : DisplayMode -> E.Value
encodeDisplayMode displayMode =
  E.string <|
    case displayMode of
      TopicD LabelOnly -> "LabelOnly"
      TopicD Detail -> "Detail"
      BoxD BlackBox -> "BlackBox"
      BoxD WhiteBox -> "WhiteBox"
      BoxD Unboxed -> "Unboxed"


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
    |> D.andThen (Set.fromList >> D.succeed)


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
      (D.map2 SetItem
        (D.field "id" D.int)
        (D.field "boxAssocId" D.int)
      )
    )


boxDecoder : D.Decoder Box
boxDecoder =
  D.map4 Box
    (D.field "id" D.int)
    (D.field "itemSetId" D.int)
    (D.field "itemProps" (boxItemDecoder |> toDictDecoder))
    (D.field "renderer" (D.string |> BoxRendererDef.decoder))


boxItemDecoder : D.Decoder ItemProps
boxItemDecoder =
  D.map2 ItemProps
    (D.field "id" D.int)
    (D.field "display" D.string |> D.andThen displayModeDecoder)


-- TODO: remove from TopicMapDef
displayModeDecoder : String -> D.Decoder DisplayMode
displayModeDecoder str =
  case str of
    "LabelOnly" -> D.succeed (TopicD LabelOnly)
    "Detail" -> D.succeed (TopicD Detail)
    "BlackBox" -> D.succeed (BoxD BlackBox)
    "WhiteBox" -> D.succeed (BoxD WhiteBox)
    "Unboxed" -> D.succeed (BoxD Unboxed)
    _ -> D.fail ("\"" ++ str ++ "\" is an invalid DisplayMode")


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
