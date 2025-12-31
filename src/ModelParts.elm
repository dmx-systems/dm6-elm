module ModelParts exposing (Id, Item, Items, ItemInfo(..), AssocIds, TopicInfo, Icon, TextSize,
  Size, SizeField(..), AssocInfo, AssocType(..), Box, Boxes, BoxId, Class, Delta, BoxItems,
  BoxPath, rootBoxId, BoxItem, Visibility(..), Pinned(..), ItemProps(..), TopicProps,
  AssocProps, DisplayMode(..), TopicDisplay(..), BoxDisplay(..), Point, Rectangle, ImageId,
  Attributes, encodeItem, encodeBox, itemDecoder, boxDecoder, toDictDecoder)

import Dict exposing (Dict)
import Html exposing (Attribute)
import Json.Decode as D
import Json.Encode as E
import Set exposing (Set)



-- TYPES


-- Item

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


type alias Items = Dict Id Item

type alias Id = Int
type alias AssocIds = Set Id
type alias Icon = String -- name of feather icon, https://feathericons.com
type alias ImageId = Int
type alias Attributes msg = List (Attribute msg)



-- Box

type alias Box =
  { id : BoxId
  , rect : Rectangle
  , scroll : Point
  , items : BoxItems
  }


type alias Boxes = Dict Id Box

type alias BoxId = Id
type alias Class = String -- a CSS class, e.g. "dmx-topic"
type alias Delta = Point
type alias BoxItems = Dict Id BoxItem
type alias BoxPath = List BoxId


rootBoxId : BoxId
rootBoxId = 0


type alias BoxItem =
  { id : Id
  , boxAssocId : Id
  , visibility : Visibility
  , props : ItemProps
  }


type Visibility
  = Visible Pinned
  | Removed


type Pinned
  = Pinned
  | Unpinned


type ItemProps
  = TopicP TopicProps
  | AssocP AssocProps


type alias TopicProps =
  { pos : Point
  , displayMode : DisplayMode -- serialized as "display", TODO: rename to "display"?
  }


type alias AssocProps =
  {}


type DisplayMode
  = TopicD TopicDisplay
  | BoxD BoxDisplay


type TopicDisplay
  = LabelOnly
  | Detail


type BoxDisplay
  = BlackBox
  | WhiteBox
  | Unboxed


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
  E.string
    ( case assocType of
        Hierarchy -> "Hierarchy"
        Crosslink -> "Crosslink"
    )


encodeBox : Box -> E.Value
encodeBox box =
  E.object
    [ ("id", E.int box.id)
    , ("rect", E.object
        [ ("x1", E.int box.rect.x1)
        , ("y1", E.int box.rect.y1)
        , ("x2", E.int box.rect.x2)
        , ("y2", E.int box.rect.y2)
        ]
      )
    , ("scroll", E.object
        [ ("x", E.int box.scroll.x)
        , ("y", E.int box.scroll.y)
        ]
      )
    , ("items", box.items |> Dict.values |> E.list encodeBoxItem)
    ]


encodeBoxItem : BoxItem -> E.Value
encodeBoxItem item =
  E.object
    [ ("id", E.int item.id)
    , ("boxAssocId", E.int item.boxAssocId)
    , ("visibility", encodeVisibility item.visibility)
    , case item.props of
        TopicP topicProps ->
          ( "topicProps"
          , E.object
            [ ("pos", E.object
                [ ("x", E.int topicProps.pos.x)
                , ("y", E.int topicProps.pos.y)
                ]
              )
            , ("display", encodeDisplayName topicProps.displayMode)
            ]
          )
        AssocP assosProps ->
          ( "assocProps"
          , E.object []
          )
    ]


encodeVisibility : Visibility -> E.Value
encodeVisibility visibility =
  E.string
    (case visibility of
      Visible Pinned -> "Pinned"
      Visible Unpinned -> "Visible"
      Removed -> "Removed"
    )


encodeDisplayName : DisplayMode -> E.Value
encodeDisplayName displayMode =
  E.string
    (case displayMode of
      TopicD LabelOnly -> "LabelOnly"
      TopicD Detail -> "Detail"
      BoxD BlackBox -> "BlackBox"
      BoxD WhiteBox -> "WhiteBox"
      BoxD Unboxed -> "Unboxed"
    )


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


boxDecoder : D.Decoder Box
boxDecoder =
  D.map4 Box
    (D.field "id" D.int)
    (D.field "rect" <| D.map4 Rectangle
      (D.field "x1" D.int)
      (D.field "y1" D.int)
      (D.field "x2" D.int)
      (D.field "y2" D.int)
    )
    (D.field "scroll" <| D.map2 Point
      (D.field "x" D.int)
      (D.field "y" D.int)
    )
    (D.field "items" (D.list boxItemDecoder |> D.andThen toDictDecoder))


boxItemDecoder : D.Decoder BoxItem
boxItemDecoder =
  D.map4 BoxItem
    (D.field "id" D.int)
    (D.field "boxAssocId" D.int)
    (D.field "visibility" D.string |> D.andThen visibilityDecoder)
    (D.oneOf
      [ D.field "topicProps" <| D.map TopicP <| D.map2 TopicProps
        (D.field "pos" <| D.map2 Point
          (D.field "x" D.int)
          (D.field "y" D.int)
        )
        (D.field "display" D.string |> D.andThen displayModeDecoder)
      , D.field "assocProps" <| D.succeed (AssocP AssocProps)
      ]
    )


toDictDecoder : List { item | id : Id } -> D.Decoder (Dict Int { item | id : Id })
toDictDecoder items =
  items
    |> List.map (\item -> (item.id, item))
    |> Dict.fromList
    |> D.succeed


visibilityDecoder : String -> D.Decoder Visibility
visibilityDecoder str =
  case str of
    "Pinned" -> D.succeed (Visible Pinned)
    "Visible" -> D.succeed (Visible Unpinned)
    "Removed" -> D.succeed (Removed)
    _ -> D.fail <| "\"" ++ str ++ "\" is an invalid Visibility"


displayModeDecoder : String -> D.Decoder DisplayMode
displayModeDecoder str =
  case str of
    "LabelOnly" -> D.succeed (TopicD LabelOnly)
    "Detail" -> D.succeed (TopicD Detail)
    "BlackBox" -> D.succeed (BoxD BlackBox)
    "WhiteBox" -> D.succeed (BoxD WhiteBox)
    "Unboxed" -> D.succeed (BoxD Unboxed)
    _ -> D.fail <| "\"" ++ str ++ "\" is an invalid DisplayMode"


maybeString : String -> D.Decoder (Maybe String)
maybeString str =
  D.succeed
    ( case str of
        "" -> Nothing
        _ -> Just str
    )
