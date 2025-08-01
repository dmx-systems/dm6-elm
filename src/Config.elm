module Config exposing (..)

import Model exposing (..)



-- CONFIG


mainFont = "sans-serif"
mainFontSize = 14

topicSize = Size 156 28
topicDetailSize = Size 300
  (topicLineHeight * mainFontSize + 2 * (topicDetailPadding + topicBorderWidth))
topicDetailPadding = 8
topicLineHeight = 1.5
topicDefaultText = "New Topic"
topicIconSize = 16
topicBorderWidth = 1
topicRadius = 7

assocWith = 1.5
assocRadius = 14 -- should not bigger than half topicSize height
assocColor = "black"
assocDelayMillis = 200

whiteBoxRange = Size 250 150
whiteBoxRadius = 14
whiteBoxPadding = 12

blackBoxOffset = 5
