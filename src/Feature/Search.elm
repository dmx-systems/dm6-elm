module Feature.Search exposing (..)

import ModelParts exposing (Id, BoxId)



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
  = Topics (List Id) (Maybe Id) -- hovered topic
  | RelTopics (List (Id, Id)) (Maybe (Id, Id)) -- hovered related topic (topic ID, assoc ID)
  | NoSearch


type Msg
  -- Search
  = Input String
  | InputFocused
  | TopicHovered Id
  | TopicUnhovered Id
  | TopicClicked Id
  -- Traverse
  | RelTopicHovered (Id, Id)
  | RelTopicUnhovered (Id, Id)
  | RelTopicClicked (Id, Id)
  -- Fullscreen (Search & Traverse)
  | Fullscreen BoxId
