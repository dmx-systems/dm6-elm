module Feature.SearchDef exposing (..)

import ModelBase exposing (..)



type alias Model =
  { term : String
  , result : SearchResult
  }


init : Model
init =
  { term = ""
  , result = NoSearch
  }


type SearchResult
  = Topics (List TopicId) (Maybe TopicId) -- hovered topic
  | RelTopics (List (TopicId, AssocId)) (Maybe (TopicId, AssocId)) -- hovered related topic
  | NoSearch


type Msg
  -- Search
  = Input String
  | InputFocused
  | TopicHovered TopicId
  | TopicUnhovered TopicId
  | TopicClicked TopicId
  -- Traverse
  | RelTopicHovered (TopicId, AssocId)
  | RelTopicUnhovered (TopicId, AssocId)
  | RelTopicClicked (TopicId, AssocId)
  -- Fullscreen (Search & Traverse)
  | Fullscreen BoxId
