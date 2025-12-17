module Feature.Nav exposing (..)

import Browser exposing (UrlRequest)
import Url exposing (Url)



type Msg
  = UrlChanged Url -- TODO
  | LinkClicked UrlRequest -- TODO
