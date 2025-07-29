module Config exposing (..)

import Model exposing (..)
import AutoExpand



-- CONFIG


mainFontSize = 14

assocWith = 1.5
assocRadius = 14 -- should not bigger than half topic height
assocColor = "black"

dragThresholdMillis = 200

topicSize = Size 156 28
topicDetailWidth = 300
topicIconSize = 16
topicBorderWidth = 1
topicRadius = 7

blackBoxOffset = 5

whiteBoxRange = Size 250 150
whiteBoxRadius = 14
whiteBoxPadding = 12

-- topic detail text
textPadding = 8
lineHeight = 1.4
autoExpandConfig = AutoExpand.config
  { onInput = Edit << AutoExpandInput
  , padding = textPadding
  , lineHeight = lineHeight * mainFontSize
  , minRows = 1
  , maxRows = 32
  }
