module Config exposing (..)

import Model exposing (..)



-- CONFIG


mainFontSize = 14

assocWith = 1.5
assocRadius = 14 -- should not bigger than half topic height
assocColor = "black"

dragThresholdMillis = 200

topicSize = Size 156 28
topicDetailSize = Size 300 (lineHeight * mainFontSize + 2 * (textPadding + topicBorderWidth))
topicDefaultText = "New Topic"
topicIconSize = 16
topicBorderWidth = 1
topicRadius = 7

blackBoxOffset = 5

whiteBoxRange = Size 250 150
whiteBoxRadius = 14
whiteBoxPadding = 12

-- topic detail text
textPadding = 8
lineHeight = 1.5
