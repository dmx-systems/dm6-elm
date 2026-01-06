module Feature.Tool exposing (..)

import ModelParts exposing (Id, BoxId)

import Json.Decode as D
import Json.Encode as E



type alias Model =
  { menu : Bool
  , lineStyle : LineStyle
  }


init : Model
init =
  { menu = False
  , lineStyle = Cornered
  }


type LineStyle
  = Cornered
  | Straight


type Msg
  -- Global Tools
  = Home
  | Menu
  | Set LineStyle
  | Import
  | Export
  -- Map Tools
  | AddTopic
  | AddBox
  | Undo
  | Redo
  -- Item Tools
  | Edit
  | Icon
  | Traverse
  | Delete
  | Remove
  | Fullscreen BoxId -- box only
  | Unbox BoxId BoxId -- box only
  | ToggleDisplay Id BoxId
  -- Text Tools
  | Image Id
  | LeaveEdit



-- JSON


encode : Model -> E.Value
encode model =
  E.object
    [ ( "lineStyle", encodeLineStyle model.lineStyle ) ]


encodeLineStyle : LineStyle -> E.Value
encodeLineStyle lineStyle =
  E.string <|
    case lineStyle of
      Cornered -> "Cornered"
      Straight -> "Straight"


decoder : D.Decoder Model
decoder =
  D.field "lineStyle" D.string
    |> D.andThen lineStyleDecoder
    |> D.andThen (\lineStyle -> D.succeed { init | lineStyle = lineStyle})


lineStyleDecoder : String -> D.Decoder LineStyle
lineStyleDecoder lineStyle =
  case lineStyle of
    "Cornered" -> D.succeed Cornered
    "Straight" -> D.succeed Straight
    _ -> D.fail <| "\"" ++ lineStyle ++ "\" is an invalid LineStyle"
