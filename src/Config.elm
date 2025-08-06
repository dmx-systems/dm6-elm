module Config exposing (..)

import Model exposing (..)



-- CONFIG


homeMapName = "DM6 Elm"
newTopicPos = Point 178 107

mainFont = "sans-serif"
toolbarFontSize = 14
contentFontSize = 13
footerFontSize = 13

topicWidth = 156
topicHeight = 28 -- also width/height of square icon box
topicW2 = topicWidth / 2
topicH2 = topicHeight / 2
topicSize = Size topicWidth topicHeight
topicLabelWeight = "bold" -- or "normal"
topicDetailSize = Size
  (topicWidth - topicHeight) -- detail width does not include icon box
  (topicLineHeight * contentFontSize + 2 * (topicDetailPadding + topicBorderWidth))
topicDetailMaxWidth = 300
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
