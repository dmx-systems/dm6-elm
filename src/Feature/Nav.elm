module Feature.Nav exposing (..)

import Browser exposing (UrlRequest)
import Browser.Navigation exposing (Key)
import Url exposing (Url)



type alias Model =
  { key : Key }


init : Key -> Model
init key =
  { key = key }


type Msg
  = UrlChanged Url
  | LinkClicked UrlRequest
