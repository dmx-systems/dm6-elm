module Config exposing (..)

import ModelParts exposing (Point, Size)



-- CONFIG


rootBoxName = "DM6 Elm"
version = "0.3-snapshot"
date = "Nov 30, 2025"

appHeaderHeight = 36

mainFont = "sans-serif"
toolFontSize = 13
contentFontSize = 13
footerFontSize = 13

toolbarColor = "#e9e9ed" -- Firefox button color
disabledColor = "gray"
limboColor = "gray"

initTopicPos = Point 232 167
initTopicText = "New Topic"
initBoxText = "New Box"
nestedBoxOffset = Point 400 300

topicWidth = 156
topicHeight = 28 -- also width/height of square icon box
topicW2 = topicWidth / 2
topicH2 = topicHeight / 2
topicSize = Size topicWidth topicHeight
topicLabelWeight = "bold" -- "normal"
topicDetailSize = Size
  (topicWidth - topicHeight) -- detail width does not include icon box
  (topicLineHeight * contentFontSize + 2 * (topicDetailPadding + topicBorderWidth))
topicDetailMaxWidth = 300
topicDetailPadding = 8
topicLineHeight = 1.5
topicIconSize = 16
topicBorderWidth = 1
topicRadius = 7

assocWidth = 1.5
assocRadius = 14 -- should not be bigger than half topicSize height
assocColor = "black"
assocDelayMillis = 200

whiteBoxRange = Size 250 150
whiteBoxRadius = 14
whiteBoxPadding = 12

blackBoxOffset = 5
