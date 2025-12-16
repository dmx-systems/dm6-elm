module ModelParts exposing (Item, ItemInfo(..), TopicInfo, AssocInfo, Items, Id, AssocIds, Icon,
  Size, SizeField(..), TextSize, ItemType, RoleType, Box, Boxes, BoxId, Class, Delta, BoxItems,
  BoxPath, rootBoxId, BoxItem, ViewProps(..), TopicProps, AssocProps, DisplayMode(..),
  TopicDisplay(..), BoxDisplay(..), Point, Rectangle, ImageId, encodeItem, encodeBox,
  itemDecoder, boxDecoder, toDictDecoder)

import Dict exposing (Dict)
import Json.Decode as D
import Json.Encode as E
import Set exposing (Set)



-- TYPES


-- Item

type alias Item =
  { id : Id
  , info : ItemInfo
  , assocIds : AssocIds
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
  , itemType : ItemType -- serialized as "type", field can't be named "type", a reserved word
  , role1 : RoleType
  , player1 : Id
  , role2 : RoleType
  , player2 : Id
  }


type alias TextSize =
  { view: Size
  , editor: Size
  }


type SizeField
  = View
  | Editor


type alias Size =
  { w : Float
  , h : Float
  }


type alias Items = Dict Id Item

type alias Id = Int
type alias AssocIds = Set Id
type alias Icon = String -- name of feather icon, https://feathericons.com
type alias ItemType = String -- a type URI, e.g. "dmx.association"
type alias RoleType = String -- a role type URI, e.g. "dmx.default"
type alias ImageId = Int


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
  , parentAssocId : Id -- TODO: drop it? Compute from Item's "assocIds" field instead?
  , hidden : Bool -- TODO: replace hidden/pinned by custom type: Hidden/Visible/Pinned?
  , pinned : Bool
  , props : ViewProps
  }


type ViewProps
  = TopicV TopicProps
  | AssocV AssocProps


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
  { x : Float
  , y : Float
  }


type alias Rectangle =
  { x1 : Float
  , y1 : Float
  , x2 : Float
  , y2 : Float
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
          , ("type", E.string assoc.itemType)
          , ("role1", E.string assoc.role1)
          , ("player1", E.int assoc.player1)
          , ("role2", E.string assoc.role2)
          , ("player2", E.int assoc.player2)
          , ("assocIds", E.set E.int item.assocIds)
          ]
        )
    ]


encodeTextSize : TextSize -> E.Value
encodeTextSize size =
  E.object
    [ ("view", E.object
        [ ("w", E.float size.view.w)
        , ("h", E.float size.view.h)
        ]
      )
    , ("editor", E.object
        [ ("w", E.float size.editor.w)
        , ("h", E.float size.editor.h)
        ]
      )
    ]


encodeBox : Box -> E.Value
encodeBox box =
  E.object
    [ ("id", E.int box.id)
    , ("rect", E.object
        [ ("x1", E.float box.rect.x1)
        , ("y1", E.float box.rect.y1)
        , ("x2", E.float box.rect.x2)
        , ("y2", E.float box.rect.y2)
        ]
      )
    , ("scroll", E.object
        [ ("x", E.float box.scroll.x)
        , ("y", E.float box.scroll.y)
        ]
      )
    , ("items", box.items |> Dict.values |> E.list encodeBoxItem)
    ]


encodeBoxItem : BoxItem -> E.Value
encodeBoxItem item =
  E.object
    [ ("id", E.int item.id)
    , ("parentAssocId", E.int item.parentAssocId)
    , ("hidden", E.bool item.hidden)
    , ("pinned", E.bool item.pinned)
    , case item.props of
        TopicV topicProps ->
          ( "topicProps"
          , E.object
            [ ("pos", E.object
                [ ("x", E.float topicProps.pos.x)
                , ("y", E.float topicProps.pos.y)
                ]
              )
            , ("display", encodeDisplayName topicProps.displayMode)
            ]
          )
        AssocV assosProps ->
          ( "assocProps"
          , E.object []
          )
    ]


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
        (D.map Assoc <| D.map6 AssocInfo
          (D.field "id" D.int)
          (D.field "type" D.string)
          (D.field "role1" D.string)
          (D.field "player1" D.int)
          (D.field "role2" D.string)
          (D.field "player2" D.int)
        )
        assocIdsDecoder
      )
    ]


textSizeDecoder : D.Decoder TextSize
textSizeDecoder =
  (D.field "size" <| D.map2 TextSize
    (D.field "view" <| D.map2 Size
      (D.field "w" D.float)
      (D.field "h" D.float)
    )
    (D.field "editor" <| D.map2 Size
      (D.field "w" D.float)
      (D.field "h" D.float)
    )
  )


assocIdsDecoder : D.Decoder AssocIds
assocIdsDecoder =
  D.field "assocIds" (D.list D.int)
  |> D.andThen (Set.fromList >> D.succeed)


boxDecoder : D.Decoder Box
boxDecoder =
  D.map4 Box
    (D.field "id" D.int)
    (D.field "rect" <| D.map4 Rectangle
      (D.field "x1" D.float)
      (D.field "y1" D.float)
      (D.field "x2" D.float)
      (D.field "y2" D.float)
    )
    (D.field "scroll" <| D.map2 Point
      (D.field "x" D.float)
      (D.field "y" D.float)
    )
    (D.field "items" (D.list boxItemDecoder |> D.andThen toDictDecoder))


boxItemDecoder : D.Decoder BoxItem
boxItemDecoder =
  D.map5 BoxItem
    (D.field "id" D.int)
    (D.field "parentAssocId" D.int)
    (D.field "hidden" D.bool)
    (D.field "pinned" D.bool)
    (D.oneOf
      [ D.field "topicProps" <| D.map TopicV <| D.map2 TopicProps
        (D.field "pos" <| D.map2 Point
          (D.field "x" D.float)
          (D.field "y" D.float)
        )
        (D.field "display" D.string |> D.andThen displayModeDecoder)
      , D.field "assocProps" <| D.succeed (AssocV AssocProps)
      ]
    )


toDictDecoder : List { item | id : Id } -> D.Decoder (Dict Int { item | id : Id })
toDictDecoder items =
  items
  |> List.map (\item -> (item.id, item))
  |> Dict.fromList
  |> D.succeed


displayModeDecoder : String -> D.Decoder DisplayMode
displayModeDecoder str =
  case str of
    "LabelOnly" -> D.succeed (TopicD LabelOnly)
    "Detail" -> D.succeed (TopicD Detail)
    "BlackBox" -> D.succeed (BoxD BlackBox)
    "WhiteBox" -> D.succeed (BoxD WhiteBox)
    "Unboxed" -> D.succeed (BoxD Unboxed)
    _ -> D.fail <| "\"" ++ str ++ "\" is an invalid display mode"


maybeString : String -> D.Decoder (Maybe String)
maybeString str =
  D.succeed
    ( case str of
        "" -> Nothing
        _ -> Just str
    )
