module MapRenderer exposing (viewMap)

import IconMenu exposing (viewTopicIcon)
import Model exposing (..)
import Style exposing (..)

import Dict exposing (Dict)
import Html exposing (Html, Attribute, div, text, input)
import Html.Attributes exposing (id, attribute, value)
import Html.Events exposing (onInput)
import String exposing (fromInt)
import Svg exposing (Svg, svg)
import Svg.Attributes exposing (width, height, viewBox)



-- CONFIG


lineFunc = taxiLine -- directLine



-- MODEL


type alias MapInfo =
  ( ( List (Html Msg), List (Svg Msg) )
  , Rectangle
  , ( { w: String, h: String }
    , List (Attribute Msg)
    , List (Attribute Msg)
    )
  )

type alias TopicRendering = (List (Attribute Msg), List (Html Msg))



-- VIEW


viewMap : MapId -> MapId -> Model -> Html Msg
viewMap mapId parentMapId model =
  let
    ((topics, assocs), mapRect, (svgSize, svgAttr, mapStyle)) = mapInfo mapId parentMapId model
  in
  div
    mapStyle
    [ div
        (topicLayerStyle mapRect)
        topics
    , svg
        ( [ width svgSize.w, height svgSize.h ]
          ++ topicAttr mapId parentMapId
          ++ svgAttr
          ++ svgStyle
        )
        ( assocs
          ++ viewLimboAssoc mapId model
        )
    ]


mapInfo : MapId -> MapId -> Model -> MapInfo
mapInfo mapId parentMapId model =
  let
    isTopLevel = mapId == model.activeMap
  in
  case getMap mapId model.maps of
    Just map ->
      ( viewItems map model
      , map.rect
      , if isTopLevel then
          ( {w = "100%", h = "100%"}, [], [] )
        else
          let
            vb =
              { x = map.rect.x1 |> round |> fromInt
              , y = map.rect.y1 |> round |> fromInt
              , w = (map.rect.x2 - map.rect.x1) |> round |> fromInt
              , h = (map.rect.y2 - map.rect.y1) |> round |> fromInt
              }
          in
          ( {w = vb.w, h = vb.h}
          , [ viewBox (vb.x ++ " " ++ vb.y ++ " " ++ vb.w ++ " " ++ vb.h) ]
          , whiteBoxStyle mapId map.rect parentMapId model
          )
      )
    Nothing ->
      ( ([], []), Rectangle 0 0 0 0, ( {w = "0", h = "0"}, [], [] ))


viewItems : Map -> Model -> (List (Html Msg), List (Svg Msg))
viewItems map model =
  map.items |> Dict.values |> List.filter isVisible |> List.foldr
    (\{id, viewProps} (t, a) ->
      let
        item = model.items |> Dict.get id
      in
      case (item, viewProps) of
        (Just (Topic topic), ViewTopic props) -> (viewTopic topic props map.id model :: t, a)
        (Just (Assoc assoc), ViewAssoc _) -> (t, viewAssoc assoc map.id model :: a)
        _ -> logError "viewItems" ("problem with item " ++ fromInt id) (t, a)
    )
    ([], [])


viewTopic : TopicInfo -> TopicProps -> MapId -> Model -> Html Msg
viewTopic topic props mapId model =
  let
    (style, children) =
      case props.displayMode of
        Just BlackBox -> blackBoxTopic topic props mapId model
        Just WhiteBox -> whiteBoxTopic topic props mapId model
        Just Unboxed -> unboxedTopic topic props mapId model
        Nothing -> genericTopic topic props mapId model
  in
  div
    ( topicAttr topic.id mapId
      ++ topicStyle topic mapId model
      ++ style
    )
    children


blackBoxTopic : TopicInfo -> TopicProps -> MapId -> Model -> TopicRendering
blackBoxTopic topic props mapId model =
  ( topicPosStyle props
  , [ div
        (topicFlexboxStyle topic props mapId model ++ blackBoxStyle)
        (genericTopicHtml topic props model ++ viewItemCount topic.id props model)
    , div
        (ghostTopicStyle topic mapId model)
        []
    ]
  )


whiteBoxTopic : TopicInfo -> TopicProps -> MapId -> Model -> TopicRendering
whiteBoxTopic topic props mapId model =
  let
    (style, children) = genericTopic topic props mapId model
    rect =
      case getMap topic.id model.maps of
        Just map -> map.rect
        Nothing -> Rectangle 0 0 0 0
  in
  ( style
  , children
      ++ viewItemCount topic.id props model
      ++ [ viewMap topic.id mapId model ]
  )


unboxedTopic : TopicInfo -> TopicProps -> MapId -> Model -> TopicRendering
unboxedTopic topic props mapId model =
  let
    (style, children) = genericTopic topic props mapId model
  in
  ( style
  , children
      ++ viewItemCount topic.id props model
  )


genericTopic : TopicInfo -> TopicProps -> MapId -> Model -> TopicRendering
genericTopic topic props mapId model =
  ( topicPosStyle props
      ++ topicFlexboxStyle topic props mapId model
      ++ selectionStyle topic.id mapId model
  , genericTopicHtml topic props model
  )


genericTopicHtml : TopicInfo -> TopicProps -> Model -> List (Html Msg)
genericTopicHtml topic props model =
  let
    textElem =
      if model.editState /= ItemEdit topic.id then
        div
          topicLabelStyle
          [ text topic.text ]
      else
        input
          ( [ id ("dmx-input-" ++ fromInt topic.id)
            , value topic.text
            , onInput (ItemEditInput >> Edit)
            , onEnterOrEsc (Edit ItemEditEnd)
            , stopPropagationOnMousedown
            ]
            ++ topicInputStyle
          )
          []
  in
  [ div
      (topicIconBoxStyle props)
      [ viewTopicIcon topic.id model ]
  , textElem
  ]


viewItemCount : Id -> TopicProps -> Model -> List (Html Msg)
viewItemCount topicId props model =
  let
    itemCount =
      if props.displayMode /= Nothing then
        case getMap topicId model.maps of
          Just map -> map.items |> Dict.values |> List.filter isVisible |> List.length
          Nothing -> 0
      else
        0
  in
  [ div
      itemCountStyle
      [ text <| fromInt itemCount ]
  ]


topicAttr : Id -> MapId -> List (Attribute Msg)
topicAttr id mapId =
  case id of
    0 -> [] -- top-level map requires dedicated event handling, TODO
    _ ->
      [ attribute "class" "dmx-topic"
      , attribute "data-id" (fromInt id)
      , attribute "data-map-id" (fromInt mapId)
      ]


viewAssoc : AssocInfo -> MapId -> Model -> Svg Msg
viewAssoc assoc mapId model =
  let
    geom = assocGeometry assoc mapId model
  in
  case geom of
    Just (pos1, pos2) -> lineFunc (Just assoc) pos1 pos2
    Nothing -> text "" -- TODO


viewLimboAssoc : MapId -> Model -> List (Svg Msg)
viewLimboAssoc mapId model =
  case model.dragState of
    Drag DrawAssoc topicId mapId_ _ pos _ ->
      if mapId_ == mapId then
        let
          points = Maybe.map2
            (\pos1 pos2 -> (pos1, pos2))
            (topicPos topicId mapId model.maps)
            (relPos pos mapId model)
        in
        case points of
          Just (pos1, pos2) -> [ lineFunc Nothing pos1 pos2 ]
          Nothing -> []
      else
        []
    _ -> []


assocGeometry : AssocInfo -> MapId -> Model -> Maybe (Point, Point)
assocGeometry assoc mapId model =
  let
    pos1 = topicPos assoc.player1 mapId model.maps
    pos2 = topicPos assoc.player2 mapId model.maps
  in
  case Maybe.map2 (\p1 p2 -> ( p1, p2 )) pos1 pos2 of
    Just geometry -> Just geometry
    Nothing -> fail "assocGeometry" { assoc = assoc, mapId = mapId } Nothing


{-| Transforms an absolute screen position to a map-relative position.
-}
relPos : Point -> MapId -> Model -> Maybe Point
relPos pos mapId model =
  absMapPos mapId (Point 0 0) model |> Maybe.andThen
    (\posAbs -> Just
      (Point
        (pos.x - posAbs.x)
        (pos.y - posAbs.y)
      )
    )


{-| Recursively calculates the absolute position of a map.
    "posAcc" is the position accumulated so far.
-}
absMapPos : MapId -> Point -> Model -> Maybe Point
absMapPos mapId posAcc model =
  if mapId == model.activeMap then
    Just posAcc
  else
    getMap mapId model.maps
      |> Maybe.andThen
        (\map -> topicPos map.id map.parentMapId model.maps
          |> Maybe.andThen
            (\mapPos_ ->
              absMapPos
                map.parentMapId
                (Point
                  (posAcc.x + mapPos_.x - topicSize.w / 2 - map.rect.x1)
                  (posAcc.y + mapPos_.y + topicSize.h / 2 - map.rect.y1)
                )
                model
            )
        )
